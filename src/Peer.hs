{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Peer where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as C
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Torrent
import           Debug.Trace
import           System.IO

import           Conduit                      (MonadThrow, mapC, mapM_C,
                                               sinkNull, yield)
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, waitAnyCancel)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.STM            (atomically)
import           Data.Binary                  (encode)
import           Data.Conduit                 (ConduitT, runConduit, (.|))
import           Data.Conduit.Attoparsec      (conduitParser, sinkParser)
import           Data.Conduit.Network         (sinkSocket, sourceSocket)
import           Data.Function                ((&))
import           Data.Map                     (Map)
import           Data.Maybe                   (isNothing)
import           Data.Set                     (Set)
import           Data.Void                    (Void)
import           Network.Socket               (Socket)

-- Library imports
import qualified Message
import qualified PiecesMgr
import qualified Types

import           InternalMessage

import           Message                      (Message)
import           PiecesMgr                    (Piece (..), pieceIx)
import           Types                        (InfoHash, PeerId)

newtype PeerM a = PeerM (ReaderT PeerEnv (LoggingT IO) a)
                  deriving ( Functor, Applicative, Monad, MonadIO
                           , MonadReader PeerEnv, MonadLogger, MonadThrow)

-- | Two way channel between Peer and PiecesMgr
type PiecesMgrChan = (TChan PeerToPiecesMgr, TChan PiecesMgrToPeer)

type PieceSet = Set Int

data PeerState = PeerState
    { isInterested :: !Bool
    , isChoking    :: !Bool
    , amInterested :: !Bool
    , amChoking    :: !Bool
    }

type PeerSourceC m = ConduitT () Message m ()
type PeerSinkC m = ConduitT Message Void m ()

data PeerEnv = PeerEnv
    { infoHash            :: !InfoHash
    , torrentInfo         :: !TorrentInfo
    , peerId              :: !PeerId
    , sourceC             :: !(PeerSourceC PeerM)
    , sinkC               :: !(PeerSinkC PeerM)
    -- | Pieces we have downloaded so far from all Peers
    -- TODO: This is a global var, refactor somehow ?
    , ourPieces           :: !(TVar PieceSet)
    , peerAlive           :: !(TVar Bool)
    -- | Interest and choking state
    , peerState           :: !(TVar PeerState)
    -- | Piece that we're currently downloading
    , maybeRequestedPiece :: !(TVar (Maybe Int))
    -- | Pieces remote peer has
    , pieces              :: !(TVar PieceSet)
    -- | Blocks for which we have Request messages sent
    , requestedBlocks     :: !(TVar PieceSet)
    -- | Data for all the blocks downloaded so far
    , blocks              :: !(TVar (Map Int BS.ByteString))
    -- | Remaining blocks to download to complete the requested piece
    , nextBlocks          :: !(TVar (Maybe (Set Int)))
    , waitingHandshake    :: !(TVar Bool)
    }

class (Monad m) => CanMessage m where
    sendMessage :: Message -> m ()

instance CanMessage PeerM where
    sendMessage m = do
        "Sending message " ++ show m & logPeer
        asks sinkC >>= \s -> runConduit (yield m .| s)

newConfig :: InfoHash -> TorrentInfo -> PeerId -> Socket -> TVar PieceSet -> IO PeerEnv
newConfig ih tinfo pid sock ourPs = PeerEnv ih tinfo pid siC soC ourPs
                                        <$> newTVarIO False
                                        <*> newTVarIO (PeerState False True False True)
                                        <*> newTVarIO Nothing
                                        <*> newTVarIO Set.empty
                                        <*> newTVarIO Set.empty
                                        <*> newTVarIO Map.empty
                                        <*> newTVarIO Nothing
                                        <*> newTVarIO True
    where siC = sourceSocket sock .| conduitParser Message.parser .| mapC snd
          soC = mapC (LBS.toStrict . encode) .| sinkSocket sock

start :: PeerEnv -> IO ()
start = run $ do
    "Sending handshake" & logPeer
    sendMessage =<< Message.Handshake <$> asks infoHash <*> asks peerId
    env <- ask
    "Starting all services" & logPeer
    void $ liftIO $ (mapM (async . flip run env) >=> waitAnyCancel) [mainLoop] --, keepAliveLoop, checkAliveLoop]
    where run (PeerM m) conf = runStdoutLoggingT $ runReaderT m conf

mainLoop :: PeerM ()
mainLoop = asks sourceC >>= \s -> runConduit $ s .| mapM_C handleMessage .| sinkNull

handleMessage :: Message -> PeerM ()
handleMessage msg = do
    case msg of
        Message.Piece{} -> "Handling Piece " & logPeer
        _               -> "Handling " ++ show msg & logPeer
    env <- ask
    liftIO $ atomically $ writeTVar (peerAlive env) True
    case msg of
        Message.KeepAlive          -> return ()
        Message.Choke              -> setIsChoking True >> cancelRequested
        Message.Unchoke            -> setIsChoking False >> updateRequested >> continueDownload
        Message.Interested         -> undefined -- Don't handle
        Message.NotInterested      -> undefined -- Don't handle
        Message.Have ix            -> addPiece ix >> updateInterest >> updateRequested
        Message.BitField s         -> forM_ s addPiece >> updateInterest >> updateRequested
        Message.Request ix off len -> undefined -- Don't handle
        Message.Piece ix off bs    -> do
            env <- ask
            r <- liftIO $ readTVarIO $ maybeRequestedPiece env
            case r of
                Nothing -> error "Was not expecting a Piece message"
                Just i  -> when (ix == i) $ do
                               -- get block index in piece
                               let blockIx = off `div` defaultBlockLength
                               -- add block data to piece
                               liftIO $ atomically $ do
                                   modifyTVar' (blocks env) (Map.insert blockIx bs)
                                   modifyTVar' (requestedBlocks env) (Set.delete blockIx)
                               done <- completedPiece -- check if we completed the piece
                               when done $ do
                                   "Piece completed" & logPeer
                                   pieceBS <- BS.concat . map snd . Map.toAscList <$> (liftIO . readTVarIO =<< asks blocks)
                                   writePiece ix pieceBS
                                   liftIO . atomically $ do
                                       writeTVar (maybeRequestedPiece env) Nothing
                                       writeTVar (blocks env) Map.empty
                                       modifyTVar' (ourPieces env) (Set.insert i)
                                   sendMessage (Message.Have i)
                                   updateRequested
                               continueDownload
                               where completedPiece = do
                                         downloaded <- length . Map.keys <$> (liftIO . readTVarIO =<< asks blocks)
                                         let blocks = pieceBlockCount (getPieceLength i $ torrentInfo env ) defaultBlockLength
                                         "Downloaded: " ++ show downloaded ++ " Piece blocks: " ++ show blocks & logPeer
                                         return $ downloaded == blocks
        Message.Cancel ix off len  -> todo -- Cancel request
        Message.Port n             -> return ()
        Message.Handshake ih pid   ->
            liftIO $ atomically $ do
                expected <- readTVar (waitingHandshake env)
                unless expected (error "Was not expecting handshake now")
                writeTVar (waitingHandshake env) False
                unless (isValidHandshake (infoHash env) msg) (error "Invalid handshake")

-- | Get the number of blocks in a piece
pieceBlockCount :: Int -> Int -> Int
pieceBlockCount pieceLength blockLength =
    let isLastPiece = (if pieceLength `mod` blockLength > 0 then 1 else 0)
     in (pieceLength `div` blockLength) + isLastPiece

writePiece :: Int -> BS.ByteString -> PeerM ()
writePiece ix bs = do
    fname <- asks (C.unpack . tName . torrentInfo)
    off <- (* fromIntegral ix) <$> asks (fromIntegral . tPieceLength . torrentInfo)
    handle <- liftIO $ openFile fname ReadWriteMode
    "Writing piece " ++ show ix ++ " at offset " ++ show off & logPeer
    liftIO $ hSeek handle AbsoluteSeek off
    liftIO $ BS.hPut handle bs
    liftIO $ hClose handle

-- | Send Request messages for the currently donwloading piece.
continueDownload :: PeerM ()
continueDownload = do
    env <- ask
    next <- liftIO $ atomically $ do
        "continueDownload" & logPeer
        requested <- readTVar (requestedBlocks env)
        "|- requested blocks so far : " ++ show requested & logPeer
        maybeNext <- readTVar (nextBlocks env)
        case maybeNext of
            Nothing -> error "No blocks to download next"
            Just s  -> do
                -- Should always have at most 10 pending requests.
                let x = Set.take (10 - Set.size requested) s
                -- Remove selected blocks from next and add them to requested
                writeTVar (nextBlocks env) (Just $ Set.difference s x)
                writeTVar (requestedBlocks env) (Set.union requested x)
                "|- next blocks are " ++ show x & logPeer
                return x
    forM_ next requestBlock
    where requestBlock :: Int -> PeerM ()
          requestBlock bix = do
              "Requesting block " ++ show bix & logPeer
              env <- ask
              r <- liftIO $ readTVarIO $ maybeRequestedPiece env
              case r of
                  Nothing  -> error "No piece selected for download"
                  Just pix -> do
                      let len = getBlockLength pix bix defaultBlockLength (torrentInfo env)
                          off = bix * defaultBlockLength
                      sendMessage $ Message.Request pix off len

defaultBlockLength :: Int
defaultBlockLength = 2 ^ 14 -- FIXME: Remove hardcoded value

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

setIsChoking :: (MonadReader PeerEnv m, MonadIO m) => Bool -> m ()
setIsChoking b = asks peerState >>= liftIO . atomically . flip modifyTVar' (\s -> s { isChoking = b })

-- | Check if we should request new piece.
-- If we are interested, not choked and not currently downloading a piece then we should start downloading a new piece.
-- Pick a random piece which we don't have but the remote peer has.
-- TODO: Pick piece based on rarity
updateRequested :: PeerM ()
updateRequested = do
    "Updating requested piece" & logPeer
    env <- ask
    amInterested <- liftIO $ amInterested <$> readTVarIO (peerState env)
    isNotChoking <- liftIO $ not . isChoking <$> readTVarIO (peerState env)
    noRequest    <- liftIO $ isNothing <$> readTVarIO (maybeRequestedPiece env)
    when (amInterested && isNotChoking && noRequest) $ do
        remotePs <- liftIO $ readTVarIO $ pieces env
        ourPs    <- liftIO $ readTVarIO $ ourPieces env
        let nextPiece = maybeNewPiece $ Set.difference remotePs ourPs
        "|- next piece is " ++ show nextPiece & logPeer
        case nextPiece of
            Nothing -> sendMessage Message.NotInterested -- Finished downloading all pieces from this peer
            Just p  -> do
                let c = pieceBlockCount (getPieceLength p (torrentInfo env)) defaultBlockLength
                liftIO $ do
                    atomically $ writeTVar (maybeRequestedPiece env) nextPiece
                    atomically $ writeTVar (nextBlocks env) (Just (Set.fromList [0..(c-1)]))
    where maybeNewPiece :: PieceSet -> Maybe Int
          maybeNewPiece s = if Set.null s then Nothing else Just $ head $ Set.toList s

-- | Function checks if peer has any pieces we need, updates our interest
-- in remote peer and informs remote peer of our interest.
updateInterest :: (MonadReader PeerEnv m, MonadIO m, CanMessage m) => m ()
updateInterest = do
    env <- ask
    (has, inform) <- liftIO $ atomically $ do
        s        <- readTVar $ peerState env
        remotePs <- readTVar $ pieces env
        ourPs    <- readTVar $ ourPieces env
        let has = not $ Set.null $ Set.difference remotePs ourPs
        writeTVar (peerState env) $ s { amInterested = has }
        return (has, amInterested s /= has)
    when inform $ sendMessage (if has then Message.Interested else Message.NotInterested)

-- | Cancel all requested blocks
cancelRequested :: (MonadReader PeerEnv m, MonadIO m) => m ()
cancelRequested = do
    env <- ask
    liftIO $ atomically $ do
        writeTVar (maybeRequestedPiece env) Nothing
        writeTVar (requestedBlocks env) Set.empty

-- | Add piece as owned by remote peer
addPiece :: (MonadReader PeerEnv m, MonadIO m) => Int -> m ()
addPiece ix = asks pieces >>= \p -> liftIO $ atomically $ modifyTVar' p (Set.insert ix)

-- | Check if handshake is valid
-- Handshake is valid if info hash and peer id match.
-- FIXME: Check peer id ?
isValidHandshake :: Types.InfoHash -> Message -> Bool
isValidHandshake ih (Message.Handshake mih _)= mih == ih

-- | Send KeepAlive message every 2 minutes to remote peer
keepAliveLoop :: (MonadReader PeerEnv m, MonadIO m, MonadLogger m, CanMessage m) => m ()
keepAliveLoop = do
    "Keeping connection alive" & logPeer
    forever $ liftIO (threadDelay timeout) >> sendMessage Message.KeepAlive
    where timeout = 1000000 * 60 * 2

-- | Check if remote peer is still alive. If not then end all computations.
checkAliveLoop :: (MonadReader PeerEnv m, MonadIO m) => m ()
checkAliveLoop = do
    "Checking for signs of life from remote" & logPeer
    env <- ask
    liftIO $ forever $ do
        let timeout = 1000000 * 60 * 2
        threadDelay timeout
        atomically $ do
            alive <- readTVar (peerAlive env)
            unless alive (error "Peer is not alive anymore")
            writeTVar (peerAlive env) False
    "End of checkAliveLoop" & logPeer

logPeer s = traceM $ mconcat ["Peer: ", s]

todo = error "TODO"
