{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Internal.Peer where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Data.Torrent
import           Debug.Trace

import           Conduit                      (await, mapM_C, sinkNull, yield)
import           Control.Monad.Fail           (MonadFail)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.STM            (STM, atomically)
import           Data.Conduit                 (ConduitT, runConduit, (.|))
import           Data.Function                ((&))
import           Data.Map                     (Map)
import           Data.Maybe                   (isNothing)
import           Data.Set                     (Set)
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Data.Void                    (Void)

import           InternalMessage              (PiecesMgrMessage (..))
import           Message                      (Message)
import           MovingWindow                 (MovingWindow)
import           Types                        (InfoHash, PeerId)

import qualified Data.ByteString              as BS
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set

import qualified Message
import qualified MovingWindow                 as MW


type PeerM = ReaderT PeerEnv IO

type PieceSet = Set Int
type BlockSet = Set Int

-- TODO: Add isInterested and amChoking fields
data PeerState = PeerState
    { isChoking    :: Bool
    , amInterested :: Bool
    }

type PeerSourceC m = ConduitT () Message m ()
type PeerSinkC m = ConduitT Message Void m ()

data PeerEnv = PeerEnv
    { peInfoHash             :: InfoHash
    , peTorrentInfo          :: TorrentInfo
    , pePeerId               :: PeerId
    , peSourceC              :: PeerSourceC PeerM
    , peSinkC                :: PeerSinkC PeerM
    -- | Pieces we have downloaded so far from all Peers
    -- TODO: This is a global var, refactor somehow ?
    , peOurPieces            :: TVar PieceSet
    , peDownloadMovingWindow :: TVar MovingWindow
    , peToPiecesMgr          :: TChan PiecesMgrMessage
    , pePeerAlive            :: TVar Bool
    -- | Interest and choking state
    , pePeerState            :: TVar PeerState
    , penvPiecesInfo         :: PiecesInfo
    , penvBlocksInfo         :: BlocksInfo
    , peWaitingHandshake     :: TVar Bool
    }

data PiecesInfo = PiecesInfo
    { pinfoRequested    :: TVar (Maybe Int)  -- Piece that we're currently downloading
    , pinfoRemotePieces :: TVar PieceSet
    }

data BlocksInfo = BlocksInfo
    { binfoRequested  :: TVar BlockSet  -- Blocks for which we have Request messages sent
    , binfoDownloaded :: TVar (Map Int BS.ByteString)  -- Data for all the peBlocks downloaded so far
    , binfoRemaining  :: TVar BlockSet  -- Remaining peBlocks to download to complete the requested piece
    }

class HasTorrentInfo a where
    torrentInfo :: a -> TorrentInfo
instance HasTorrentInfo PeerEnv where
    torrentInfo = peTorrentInfo

class HasPiecesInfo a where
    requestedPiece :: a -> TVar (Maybe Int)
    remotePieces :: a -> TVar PieceSet
instance HasPiecesInfo PeerEnv where
    requestedPiece = pinfoRequested . penvPiecesInfo
    remotePieces = pinfoRemotePieces . penvPiecesInfo

class HasBlocksInfo a where
    requestedBlocks :: a -> TVar BlockSet
    downloadedBlocks :: a -> TVar (Map Int BS.ByteString)
    remainingBlocks :: a -> TVar BlockSet
instance HasBlocksInfo PeerEnv where
    requestedBlocks = binfoRequested . penvBlocksInfo
    downloadedBlocks = binfoDownloaded . penvBlocksInfo
    remainingBlocks = binfoRemaining . penvBlocksInfo

class HasPeerState a where
    peerState :: a ->  TVar PeerState
instance HasPeerState PeerEnv where
    peerState = pePeerState

class HasLocalPieces a where
    localPieces :: a -> TVar PieceSet
instance HasLocalPieces PeerEnv where
    localPieces = peOurPieces

class HasHandshakeStatus a where
    waitingHandshake :: a -> TVar Bool
instance HasHandshakeStatus PeerEnv where
    waitingHandshake = peWaitingHandshake

class HasInfoHash a where
    infoHash :: a -> InfoHash
instance HasInfoHash PeerEnv where
    infoHash = peInfoHash

class HasPiecesMgrChan a where
    piecesMgrChan :: a -> TChan PiecesMgrMessage
instance HasPiecesMgrChan PeerEnv where
    piecesMgrChan = peToPiecesMgr

class HasDownloadMovingWindow a where
    downloadMovingWindow :: a -> TVar MovingWindow
instance HasDownloadMovingWindow PeerEnv where
    downloadMovingWindow = peDownloadMovingWindow

class (Monad m) => CanMessage m where
    sendMessage :: Message -> m ()
instance CanMessage PeerM where
    sendMessage m = do
        "Sending message " ++ show m & logPeer
        asks peSinkC >>= \s -> runConduit (yield m .| s)

mainLoop :: PeerM ()
mainLoop = do
    env <- ask
    runConduit $ peSourceC env .| handleMessages .| peSinkC env

handleMessages :: (MonadReader env m, HasDownloadMovingWindow env, HasTorrentInfo env,
                   HasPiecesInfo env, HasBlocksInfo env, HasPiecesMgrChan env,
                   HasInfoHash env, HasHandshakeStatus env, HasPeerState env,
                   HasLocalPieces env, MonadIO m, MonadFail m)
               => ConduitT Message Message m ()
handleMessages = do
    env <- ask
    "Awaiting message" & logPeer
    maybeMessage <- await
    case maybeMessage of
        Nothing  -> return ()
        Just msg -> do
            "Handling message" & logPeer
            -- liftIO $ atomically $ writeTVar (peerAlive env) True
            case msg of
                Message.KeepAlive          -> return ()
                Message.Choke              ->
                    liftIO $ atomically $ setIsChoking (peerState env) True >> cancelRequested env
                Message.Unchoke            -> do
                    liftIO $ atomically $ setIsChoking (peerState env) False
                    updateRequested >> continueDownload
                Message.Interested         -> undefined -- Don't handle
                Message.NotInterested      -> undefined -- Don't handle
                Message.Have ix            -> addPiece ix >> updateInterest >> updateRequested
                Message.BitField s         -> forM_ s addPiece >> updateInterest >> updateRequested
                Message.Request{}          -> undefined -- Don't handle
                Message.Piece ix off bs    -> do
                    Just i <- liftIO $ readTVarIO $ requestedPiece env
                    when (ix == i) $ do
                        -- get block index in piece
                        let blockIx = off `div` defaultBlockLength
                        addDownloadedBlock blockIx bs
                        done <- completedPiece -- check if we completed the piece
                        when done $ do
                            "Piece completed" & logPeer
                            pieceBS <- BS.concat . map snd . Map.toAscList
                                        <$> liftIO (readTVarIO $ downloadedBlocks env)
                            lift $ notifyPiecesMgr (HavePiece ix pieceBS)
                            liftIO $ atomically $ do
                                cancelRequested env
                                modifyTVar' (localPieces env) (Set.insert i)
                            yield (Message.Have i)
                            updateRequested
                        continueDownload
                Message.Cancel{}           -> todo -- Cancel request
                Message.Port{}             -> return ()
                Message.Handshake{}        ->
                    liftIO $ atomically $ do
                        expected <- readTVar (waitingHandshake env)
                        unless expected (error "Was not expecting handshake now")
                        writeTVar (waitingHandshake env) False
                        unless (isValidHandshake (infoHash env) msg) (error "Invalid handshake")
            handleMessages

pieceIndex :: (HasPiecesInfo env, MonadIO m) => env -> m Int
pieceIndex env = do
    mp <- liftIO $ readTVarIO (requestedPiece env)
    case mp of
        Nothing -> error "No piece requested"
        Just x  -> return x

-- TODO: Replace Int with BlockIx
-- TODO: Replace ByteString with BlockData
addDownloadedBlock :: (MonadReader env m, HasTorrentInfo env, HasPiecesInfo env,
                       HasBlocksInfo env, HasDownloadMovingWindow env, MonadIO m)
                   => Int -> BS.ByteString -> m ()
addDownloadedBlock blockIx bs = do
    env <- ask
    pix <- liftIO $ pieceIndex env
    let blen = getBlockLength pix blockIx defaultBlockLength (torrentInfo env)
    liftIO $ do
        time <- getPOSIXTime
        atomically $ modifyTVar' (downloadMovingWindow env) (`MW.insert` (time, blen))
        atomically $ do
            modifyTVar' (downloadedBlocks env) (Map.insert blockIx bs)
            modifyTVar' (requestedBlocks env) (Set.delete blockIx)

-- | Function checks if we've completed the piece we're currently requesting
completedPiece :: (MonadReader env m, MonadFail m, MonadIO m,
                   HasPiecesInfo env, HasBlocksInfo env, HasTorrentInfo env)
               => m Bool
completedPiece = do
    downloaded <- length . Map.keys <$> (asks downloadedBlocks >>= liftIO . readTVarIO)
    Just i <- asks requestedPiece >>= liftIO . readTVarIO
    blockCount <- flip pieceBlockCount defaultBlockLength . getPieceLength i <$> asks torrentInfo
    "Downloaded: " ++ show downloaded ++ " Piece peBlocks: " ++ show blockCount & logPeer
    return $ downloaded == blockCount

-- | Get the number of peBlocks in a piece
pieceBlockCount :: Int -> Int -> Int
pieceBlockCount pieceLength blockLength =
    let isLastPiece = (if pieceLength `mod` blockLength > 0 then 1 else 0)
     in (pieceLength `div` blockLength) + isLastPiece


-- | Send Request messages for the currently donwloading piece.
continueDownload :: (MonadReader env m, HasTorrentInfo env, HasPiecesInfo env,
                     HasBlocksInfo env, MonadIO m)
                 => ConduitT void Message m ()
continueDownload = do
    env <- ask
    requested <- liftIO $ readTVarIO (requestedBlocks env)
    remaining <- liftIO $ readTVarIO (remainingBlocks env)
    -- Should always have at most 10 pending requests.
    unless (null remaining) $ do
        let next = Set.take (10 - Set.size requested) remaining
        liftIO $ atomically $ do
            writeTVar (remainingBlocks env) (Set.difference remaining next)
            writeTVar (requestedBlocks env) (Set.union requested next)
        forM_ next requestBlock
    where requestBlock :: (MonadReader env m, HasTorrentInfo env,
                           HasPiecesInfo env, MonadIO m)
                       => Int -> ConduitT a Message m ()
          requestBlock bix = do
              "Requesting block " ++ show bix & logPeer
              env <- ask
              r <- liftIO $ readTVarIO $ requestedPiece env
              case r of
                  Nothing  -> error "No piece selected for download"
                  Just pix -> do
                      let len = getBlockLength pix bix defaultBlockLength (torrentInfo env)
                          off = bix * defaultBlockLength
                      yield $ Message.Request pix off len

-- | Get the length of a group of elements from
-- a collection split into groups of at most
-- 'defaultLength' sized groups.
groupLength :: Int -> Int -> Int -> Int
groupLength ix defaultLength total
    | ix == total `div` defaultLength && lastLength > 0 = lastLength
    | otherwise = defaultLength
    where lastLength = total `mod` defaultLength

-- | Get length of piece given the piece index,
-- default piece length and total length of file.
getPieceLength :: Int -> TorrentInfo -> Int
getPieceLength ix tinfo = groupLength ix (fromIntegral $ tPieceLength tinfo) (fromIntegral $ tLength tinfo)

-- | Get length in bytes of block of given by index in the piece
-- we're requesting.
getBlockLength :: Int -> Int -> Int -> TorrentInfo -> Int
getBlockLength pix bix blen tinfo = let plen = getPieceLength pix tinfo in groupLength bix blen plen

setIsChoking :: TVar PeerState -> Bool -> STM ()
setIsChoking var b = modifyTVar' var (\s -> s { isChoking = b })

-- | Check if we should request new piece.
-- If we are interested, not choked and not currently downloading a piece
-- then we should start downloading a new piece.
-- Pick a random piece which we don't have but the remote peer has.
-- TODO: Pick piece based on rarity
updateRequested :: (MonadReader env m, HasPeerState env, HasTorrentInfo env,
                    HasPiecesInfo env, HasBlocksInfo env, HasLocalPieces env, MonadIO m)
                => ConduitT a Message m ()
updateRequested = do
    "Updating requested piece" & logPeer
    env <- ask
    interested   <- liftIO $ amInterested <$> readTVarIO (peerState env)
    isNotChoking <- liftIO $ not . isChoking <$> readTVarIO (peerState env)
    noRequest    <- liftIO $ isNothing <$> readTVarIO (requestedPiece env)
    when (interested && isNotChoking && noRequest) $ do
        remotePs <- liftIO $ readTVarIO $ remotePieces env
        ourPs    <- liftIO $ readTVarIO $ localPieces env
        let nextPiece = maybeNewPiece $ Set.difference remotePs ourPs
        "|- next piece is " ++ show nextPiece & logPeer
        case nextPiece of
            -- Finished downloading all pieces from this peer
            Nothing -> yield Message.NotInterested -- >> throw PeerFinished
            Just p  -> do
                let c = pieceBlockCount (getPieceLength p (torrentInfo env)) defaultBlockLength
                liftIO $ atomically $ do
                    writeTVar (requestedPiece env) nextPiece
                    writeTVar (remainingBlocks env) (Set.fromList [0..(c-1)])
    where maybeNewPiece :: PieceSet -> Maybe Int
          maybeNewPiece s = if Set.null s then Nothing else Just $ head $ Set.toList s

-- | Function checks if peer has any pieces we need, updates our interest
-- in remote peer and informs remote peer of our interest.
updateInterest :: (MonadReader env m, HasPeerState env, HasPiecesInfo env,
                   HasLocalPieces env, MonadIO m)
               => ConduitT a Message m ()
updateInterest = do
    env <- ask
    (has, inform) <- liftIO $ atomically $ do
        s        <- readTVar $ peerState env
        remotePs <- readTVar $ remotePieces env
        ourPs    <- readTVar $ localPieces env
        let has = not $ Set.null $ Set.difference remotePs ourPs
        writeTVar (peerState env) $ s { amInterested = has }
        return (has, amInterested s /= has)
    when inform $ yield (if has then Message.Interested else Message.NotInterested)

-- | Cancel all requested peBlocks
cancelRequested :: (HasPiecesInfo env, HasBlocksInfo env) => env -> STM ()
cancelRequested env = do
    writeTVar (requestedPiece env) Nothing
    writeTVar (requestedBlocks env) Set.empty
    writeTVar (downloadedBlocks env) Map.empty

-- | Add piece as owned by remote peer
addPiece :: (MonadReader env m, MonadIO m, HasPiecesInfo env) => Int -> m ()
addPiece ix = asks remotePieces >>= liftIO . atomically . flip modifyTVar' (Set.insert ix)

notifyPiecesMgr :: (MonadReader env m, HasPiecesMgrChan env, MonadIO m) => PiecesMgrMessage -> m ()
notifyPiecesMgr msg = do
    toPiecesMgr <- asks piecesMgrChan
    liftIO $ atomically $ writeTChan toPiecesMgr msg

defaultBlockLength :: Int
defaultBlockLength = 2 ^ 14  -- FIXME: Remove hardcoded value

-- | Check if handshake is valid
-- Handshake is valid if info hash and peer id match.
-- FIXME: Check peer id ?
isValidHandshake :: Types.InfoHash -> Message -> Bool
isValidHandshake ih (Message.Handshake mih _) = mih == ih
isValidHandshake _ _                          = error "Invalid argument"

logPeer :: (Applicative m) => String -> m ()
logPeer s = traceM $ mconcat ["Peer: ", s]

todo :: m ()
todo = error "TODO"
