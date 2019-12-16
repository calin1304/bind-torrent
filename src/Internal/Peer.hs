{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Internal.Peer
    ( PeerEnv (..)
    , PieceSet
    , mainLoop
    , sendMessage
    , BlocksInfo (..)
    , PiecesInfo (..)
    , PeerState (..)
    ) where

import           Control.Concurrent.STM.TBChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Data.Torrent
import           Debug.Trace

import           Conduit                      (await, yield)
import           Control.Lens                 (forOf_, _Just)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.STM            (STM, atomically)
import           Data.Conduit                 (ConduitT, runConduit, (.|))
import           Data.Function                ((&))
import           Data.Map                     (Map)
import           Data.Maybe                   (isNothing)
import           Data.Maybe                   (fromJust)
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
    , _blockSize             :: Int
    -- | Pieces we have downloaded so far from all Peers
    -- TODO: This is a global var, refactor somehow ?
    , peOurPieces            :: TVar PieceSet
    , peDownloadMovingWindow :: TVar MovingWindow
    , peToPiecesMgr          :: TBChan PiecesMgrMessage
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

handleMessages ::ConduitT Message Message PeerM ()
handleMessages = do
    env <- ask
    lift $ "Awaiting message" & logPeer
    maybeMessage <- await
    forOf_ _Just maybeMessage $ \msg -> do
        lift $ "Handling message" & logPeer
        -- liftIO $ atomically $ writeTVar (peerAlive env) True
        case msg of
            Message.KeepAlive          -> return ()
            Message.Choke              ->
                liftIO $ atomically $ setIsChoking (pePeerState env) True >> cancelRequested env
            Message.Unchoke            -> do
                liftIO $ atomically $ setIsChoking (pePeerState env) False
                updateRequested >> continueDownload
            Message.Interested         -> undefined -- Don't handle
            Message.NotInterested      -> undefined -- Don't handle
            Message.Have ix            -> lift (addPiece ix) >> updateInterest >> updateRequested
            Message.BitField s         -> forM_ s (lift . addPiece) >> updateInterest >> updateRequested
            Message.Request{}          -> undefined -- Don't handle
            Message.Piece ix off bs    -> do
                Just i <- liftIO $ readTVarIO $ requestedPiece env
                when (ix == i) $ do
                    -- get block index in piece
                    blockIx <- asks $ (off `div`) . _blockSize  
                    lift $ addDownloadedBlock blockIx bs
                    done <- lift completedPiece -- check if we completed the piece
                    when done $ do
                        lift $ "Piece completed" & logPeer
                        pieceBS <- BS.concat . map snd . Map.toAscList
                                    <$> liftIO (readTVarIO $ downloadedBlocks env)
                        lift $ notifyPiecesMgr (HavePiece ix pieceBS)
                        liftIO $ atomically $ do
                            cancelRequested env
                            modifyTVar' (peOurPieces env) (Set.insert i)
                        yield (Message.Have i)
                        updateRequested
                    continueDownload
            Message.Cancel{}           -> todo -- Cancel request
            Message.Port{}             -> return ()
            Message.Handshake{}        ->
                liftIO $ atomically $ do
                    expected <- readTVar (peWaitingHandshake env)
                    unless expected (error "Was not expecting handshake now")
                    writeTVar (peWaitingHandshake env) False
                    unless (isValidHandshake (peInfoHash env) msg) (error "Invalid handshake")
        handleMessages

pieceIndex :: (HasPiecesInfo env, MonadIO m) => env -> m Int
pieceIndex env = liftIO $
    fromJust (error "No piece requested") <$> readTVarIO (requestedPiece env)

-- TODO: Replace Int with BlockIx
-- TODO: Replace ByteString with BlockData
addDownloadedBlock :: Int -> BS.ByteString -> PeerM ()
addDownloadedBlock blockIx bs = do
    env <- ask
    pix <- liftIO $ pieceIndex env
    let blen = getBlockLength pix blockIx (_blockSize env) (peTorrentInfo env)
    time <- liftIO getPOSIXTime
    liftIO $ atomically $ do
        modifyTVar' (peDownloadMovingWindow env) (MW.insert (time, blen))
        modifyTVar' (downloadedBlocks env) (Map.insert blockIx bs)
        modifyTVar' (requestedBlocks env) (Set.delete blockIx)

-- | Function checks if we've completed the piece we're currently requesting
completedPiece :: PeerM Bool
completedPiece = do
    downloaded <- length . Map.keys <$> (asks downloadedBlocks >>= liftIO . readTVarIO)
    Just i <- asks requestedPiece >>= liftIO . readTVarIO
    blockSize <- asks _blockSize
    blockCount <- flip pieceBlockCount blockSize . getPieceLength i <$> asks peTorrentInfo
    "Downloaded: " ++ show downloaded ++ " Piece peBlocks: " ++ show blockCount & logPeer
    return $ downloaded == blockCount

-- | Get the number of peBlocks in a piece
pieceBlockCount :: Int -> Int -> Int
pieceBlockCount pieceLength blockLength =
    let isLastPiece = (if pieceLength `mod` blockLength > 0 then 1 else 0)
     in (pieceLength `div` blockLength) + isLastPiece


-- | Send Request messages for the currently donwloading piece.
continueDownload :: ConduitT void Message PeerM ()
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
    where requestBlock :: Int -> ConduitT a Message PeerM ()
          requestBlock bix = do
              lift $ "Requesting block " ++ show bix & logPeer
              env <- ask
              r <- liftIO $ readTVarIO $ requestedPiece env
              r & maybe (error "No piece selected for download") (\pix -> do
                    blockSize <- asks _blockSize
                    let len = getBlockLength pix bix blockSize (peTorrentInfo env)
                    let off = bix * blockSize 
                    yield $ Message.Request pix off len)

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
updateRequested :: ConduitT a Message PeerM ()
updateRequested = do
    lift $ "Updating requested piece" & logPeer
    env <- ask
    interested   <- liftIO $ amInterested <$> readTVarIO (pePeerState env)
    isNotChoking <- liftIO $ not . isChoking <$> readTVarIO (pePeerState env)
    noRequest    <- liftIO $ isNothing <$> readTVarIO (requestedPiece env)
    when (interested && isNotChoking && noRequest) $ do
        remotePs <- liftIO $ readTVarIO $ remotePieces env
        ourPs    <- liftIO $ readTVarIO $ peOurPieces env
        let nextPiece = maybeNewPiece $ Set.difference remotePs ourPs
        lift $ "|- next piece is " ++ show nextPiece & logPeer
        case nextPiece of
            -- Finished downloading all pieces from this peer
            Nothing -> yield Message.NotInterested -- >> throw PeerFinished
            Just p  -> do
                c <- pieceBlockCount (getPieceLength p (peTorrentInfo env)) <$> asks _blockSize
                liftIO $ atomically $ do
                    writeTVar (requestedPiece env) nextPiece
                    writeTVar (remainingBlocks env) (Set.fromList [0..(c-1)])
    where maybeNewPiece :: PieceSet -> Maybe Int
          maybeNewPiece s = if Set.null s then Nothing else Just $ head $ Set.toList s

-- | Function checks if peer has any pieces we need, updates our interest
-- in remote peer and informs remote peer of our interest.
updateInterest :: ConduitT a Message PeerM ()
updateInterest = do
    env <- ask
    (has, inform) <- liftIO $ atomically $ do
        s        <- readTVar $ pePeerState env
        remotePs <- readTVar $ remotePieces env
        ourPs    <- readTVar $ peOurPieces env
        let has = not $ Set.null $ Set.difference remotePs ourPs
        writeTVar (pePeerState env) $ s { amInterested = has }
        return (has, amInterested s /= has)
    when inform $ yield (if has then Message.Interested else Message.NotInterested)

-- | Cancel all requested peBlocks
cancelRequested :: PeerEnv -> STM ()
cancelRequested env = do
    writeTVar (requestedPiece env) Nothing
    writeTVar (requestedBlocks env) Set.empty
    writeTVar (downloadedBlocks env) Map.empty

-- | Add piece as owned by remote peer
addPiece :: Int -> PeerM ()
addPiece ix = asks remotePieces >>= liftIO . atomically . flip modifyTVar' (Set.insert ix)

notifyPiecesMgr :: PiecesMgrMessage -> PeerM ()
notifyPiecesMgr msg =
    asks peToPiecesMgr >>= liftIO . atomically . flip writeTBChan msg

-- | Check if handshake is valid
-- Handshake is valid if info hash and peer id match.
-- FIXME: Check peer id ?
isValidHandshake :: Types.InfoHash -> Message -> Bool
isValidHandshake ih (Message.Handshake mih _) = mih == ih
isValidHandshake _ _                          = error "Invalid argument"

logPeer :: String -> PeerM ()
logPeer s = traceM $ "Peer: " <> s

todo :: m ()
todo = error "TODO"
