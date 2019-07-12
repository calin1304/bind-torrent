{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Peer where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Text                    as Text

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Torrent
import           Debug.Trace

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, waitAnyCancel)
import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.STM            (atomically)
import           Data.Binary                  (decode, encode)
import           Data.Map                     (Map)
import           Data.Maybe                   (fromJust, isNothing)
import           Data.Set                     (Set)
import           Data.Text                    (Text)

-- Network imports
import qualified Network.Simple.TCP           as TCP

import           Net.IPv4                     (IPv4 (..))
import           Network.Socket               (Socket)
import           Network.Socket.ByteString    (sendAll)

-- Conduit imports
import           Conduit                      (MonadThrow, headC, mapC, mapM_C,
                                               sinkNull)
import           Data.Conduit                 (ConduitT, runConduit, (.|))
import           Data.Conduit.Attoparsec      (conduitParser, sinkParser)
import           Data.Conduit.Combinators     (iterM)
import           Data.Conduit.Network         (sourceSocket)

-- Library imports
import qualified Handshake
import qualified Message
import qualified Types

import           Handshake                    (Handshake)
import           InternalMessage
import           Message                      (Message)
import           Types                        (InfoHash, PeerId, PieceIx,
                                               PieceOffset, PieceRequestLen)
type PeerM = ReaderT PeerEnv (LoggingT IO)

-- | Two way channel between Peer and PiecesMgr
type PiecesMgrChan = (TChan PeerToPiecesMgr, TChan PiecesMgrToPeer)

type PieceSet = Set Int

data PeerState = PeerState
    { isInterested :: !Bool
    , isChoking    :: !Bool
    , amInterested :: !Bool
    , amChoking    :: !Bool
    }

newPeerState = PeerState False True False True

data PeerEnv = PeerEnv
    { infoHash            :: !InfoHash
    , torrentInfo         :: !TorrentInfo
    , peerId              :: !PeerId
    , socket              :: !Socket
    -- | Pieces we have downloaded so far from all Peers
    -- TODO: This is a global var, refactor somehow ?
    , ourPieces           :: !(TVar PieceSet)
    , piecesMgrChan       :: !PiecesMgrChan
    , peerAlive           :: !(TVar Bool)
    , fromSelector        :: !(TChan SelectorToPeer)
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

newConfig :: InfoHash -> TorrentInfo -> PeerId -> Socket -> TVar PieceSet -> IO PeerEnv
newConfig ih tinfo pid sock ourPs = PeerEnv ih tinfo pid sock ourPs <$> pmgrChan
                                        <*> newTVarIO False
                                        <*> newTChanIO
                                        <*> newTVarIO newPeerState
                                        <*> newTVarIO Nothing
                                        <*> newTVarIO Set.empty
                                        <*> newTVarIO Set.empty
                                        <*> newTVarIO Map.empty
                                        <*> newTVarIO Nothing
                                        <*> newTVarIO True
    where pmgrChan = (,) <$> newTChanIO <*> newTChanIO

run :: PeerM a -> PeerEnv -> IO a
run m conf = runStdoutLoggingT $ runReaderT m conf

start :: PeerEnv -> IO ()
start = run $ do
    "Sending handshake" & logPeer
    hs <- sendHandshake -- >> recvHandshake
    "Validating handshake" & logPeer
    -- asks infoHash >>= \ih -> unless (isValidHandshake ih hs) (error "Invalid handshake")
    env <- ask
    "Starting all services" & logPeer
    void $ liftIO $ (mapM (async . flip run env) >=> waitAnyCancel) [mainLoop, keepAliveLoop, checkAliveLoop]

-- | Encode and send handshake to remote peer
sendHandshake :: (MonadReader PeerEnv m, MonadIO m) => m ()
sendHandshake = do
    hs <- LBS.toStrict . encode <$> (Handshake.new <$> asks infoHash <*> asks peerId)
    asks socket >>= liftIO . flip sendAll hs
    "Sent handshake" & logPeer

-- | Receive and decode handshake from remote peer
recvHandshake :: (MonadReader PeerEnv m, MonadIO m, MonadThrow m) => m Handshake
recvHandshake =
    asks socket >>= \s -> runConduit (sourceSocket s
                                   .| iterM (traceM . show)
                                   .| sinkParser Handshake.parser)

-- | Receive, decode and handle messages from remote peer
mainLoop :: PeerM ()
mainLoop = do
    "Listening for messages" & logPeer
    s <- asks socket
    runConduit $ sourceSocket s .| mainConduit .| sinkNull
    "End of main loop" & logPeer

mainConduit :: ConduitT BS.ByteString a PeerM ()
mainConduit = do
    "mainConduit" & logPeer
    whs <- asks waitingHandshake
    waitingForHandshake <- liftIO $ readTVarIO whs
    if waitingForHandshake
        then do
            "waiting for handshake" & logPeer
            liftIO $ atomically $ writeTVar whs False
            ih <- asks infoHash
            Just valid <- conduitParser Handshake.parser .| mapC (isValidHandshake ih . snd) .| headC
            unless valid $ error "Invalid handshake"
            mainConduit
        else conduitParser Message.parser .| mapM_C (handleMessage .snd)

-- | Handle messages from remote peer
handleMessage :: Message -> PeerM ()
handleMessage msg = do
    "Handling message " ++ show msg & logPeer
    env <- ask
    liftIO $ atomically $ writeTVar (peerAlive env) True
    case msg of
        Message.KeepAlive          -> return ()
        Message.Choke              -> setIsChoking True >> cancelRequested
        Message.Unchoke            -> setIsChoking False >> updateRequested
        Message.Interested         -> undefined -- Don't handle
        Message.NotInterested      -> undefined -- Don't handle
        Message.Have ix            -> addPiece (fromIntegral ix) >> updateInterest >> updateRequested
        Message.BitField bs        -> todo -- Add all pieces, adjust interest, adjust requested
        Message.Request ix off len -> undefined -- Don't handle
        Message.Piece ix off bs    -> do
            env <- ask
            r <- liftIO $ readTVarIO $ maybeRequestedPiece env
            case r of
                Nothing -> return ()
                Just i  -> when (fromIntegral ix == i) $ do
                               -- get block index from piece and offset
                               blockIx <- (fromIntegral off `div`) <$> blockSize
                               -- add block data to piece
                               liftIO $ atomically $ modifyTVar' (blocks env) (Map.insert blockIx bs)
                               cp <- completedPiece -- check if we completed the piece
                               if cp
                                   then do
                                       -- Send completed piece to PieceMgr
                                       pbs <- pieceByteString
                                       notifyPiecesMgr (DonePiece ix pbs)
                                       updateRequested
                                   else continueDownload
        Message.Cancel ix off len  -> todo -- Cancel request
        Message.Port n             -> return ()

-- | Check if currently requested piece is completed
completedPiece :: PeerM Bool
completedPiece = do
    env <- ask
    m <- liftIO $ readTVarIO (blocks env)
    let ixs = Map.toList m
    pbc <- asks $ fromIntegral . tPieceLength . torrentInfo
    return $ length ixs == pbc

-- | Concatenate all downloaded blocks into a ByteString
pieceByteString :: PeerM BS.ByteString
pieceByteString = do
    m <- asks blocks >>= liftIO . readTVarIO
    return (m & Map.toAscList & map snd & BS.concat)

requestBlock :: Int -> PeerM ()
requestBlock i = do
    env <- ask
    off <- fromIntegral . (i *) <$> blockSize
    len <- fromIntegral <$> blockSize -- FIXME: If last piece, this could be smaller
    pieceIx <- fromIntegral . fromJust <$> liftIO (readTVarIO (maybeRequestedPiece env))
    liftIO $ sendAll (socket env) (LBS.toStrict $ encode $ Message.Request pieceIx off len)

-- | Default block size in bytes
blockSize :: PeerM Int
blockSize = return (2 ^ 14) -- FIXME: If downloading last piece, this could be smaller

-- | Send message to PiecesMgr
notifyPiecesMgr :: PeerToPiecesMgr -> PeerM ()
notifyPiecesMgr m = do
    (to, _) <- asks piecesMgrChan
    liftIO $ atomically $ writeTChan to m

-- | Send Request messages for the currently donwloading piece.
-- Should always have at most 10 pending requests.
continueDownload :: PeerM ()
continueDownload = do
    -- Send requests until we have 10 request in buffer
    env <- ask
    next <- liftIO $ atomically $ do
        requested <- readTVar (requestedBlocks env)
        -- Always have at most 10 pending requests
        next <- Set.take (10 - Set.size requested) . fromJust <$> readTVar (nextBlocks env)
        writeTVar (requestedBlocks env) (Set.union requested next)
        return next
    forM_ next requestBlock

setIsChoking :: (MonadReader PeerEnv m, MonadIO m) => Bool -> m ()
setIsChoking b = asks peerState >>= liftIO . atomically . flip modifyTVar (\s -> s { isChoking = b })

-- | Check if we should request new piece.
-- If we are interested, not choked and not currently downloading a piece then we should start downloading a new piece.
-- Pick a random piece which we don't have but the remote peer has.
-- TODO: Pick piece based on rarity
updateRequested :: (MonadReader PeerEnv m, MonadIO m) => m ()
updateRequested = do
    env <- ask
    liftIO $ atomically $ do
        amInterested <- amInterested <$> readTVar (peerState env)
        isNotChoking <- not . isChoking <$> readTVar (peerState env)
        noRequest <- isNothing <$> readTVar (maybeRequestedPiece env)
        when (amInterested && isNotChoking && noRequest) $ do
            remotePs <- readTVar $ pieces env
            ourPs <- readTVar $ ourPieces env
            let nextPiece = pickNewPiece $ Set.difference remotePs ourPs
            writeTVar (maybeRequestedPiece env) (Just nextPiece)
    where pickNewPiece :: PieceSet -> Int
          pickNewPiece = head . Set.toList

-- | Function checks if peer has any pieces we need, updates our interest
-- in remote peer and informs remote peer of our interest.
updateInterest :: (MonadReader PeerEnv m, MonadIO m) => m ()
updateInterest = do
    env <- ask
    (has, inform) <- liftIO $ atomically $ do
        s <- readTVar $ peerState env
        remotePs <- readTVar $ pieces env
        ourPs <- readTVar $ ourPieces env
        let has = remoteHasPieces remotePs ourPs
        writeTVar (peerState env) $ s { amInterested = has }
        return (has, amInterested s /= has)
    when inform $ sendMessage (if has then Message.Interested else Message.NotInterested)
    where remoteHasPieces :: PieceSet -> PieceSet -> Bool
          remoteHasPieces remote our = not $ Set.null $ Set.difference remote our

-- | Cancel all requested blocks
cancelRequested :: (MonadReader PeerEnv m, MonadIO m) => m ()
cancelRequested = do
    env <- ask
    liftIO $ atomically $ do
        writeTVar (maybeRequestedPiece env) Nothing
        writeTVar (requestedBlocks env) Set.empty

-- | Add piece as owned by remote peer
addPiece :: (MonadReader PeerEnv m, MonadIO m) => Int -> m ()
addPiece ix = asks pieces >>= \p -> liftIO $ atomically $ modifyTVar p (Set.insert ix)

-- | Check if handshake is valid
-- Handshake is valid if info hash and peer id match.
-- FIXME: Check peer id ?
isValidHandshake :: Types.InfoHash -> Handshake -> Bool
isValidHandshake ih h = h ^. Handshake.infoHash == ih

-- | Send message to remote peer
sendMessage :: (MonadReader PeerEnv m, MonadIO m) => Message -> m ()
sendMessage msg = do
    "Sending message " ++ show msg & logPeer
    asks socket >>= send'
    where send' sock = liftIO $ sendAll sock $ LBS.toStrict $ encode msg

-- | Send KeepAlive message every 2 minutes to remote peer
keepAliveLoop :: (MonadReader PeerEnv m, MonadIO m, MonadLogger m) => m ()
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
