{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Protocol where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Text                    as Text

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Debug.Trace

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, waitAnyCancel)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.STM            (atomically)
import           Data.Binary                  (decode, encode)
import           Data.Map                     (Map)
import           Data.Maybe                   (fromJust, isNothing)
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import           Data.Torrent                 (TorrentInfo, tPieceLength)

-- Network imports
import qualified Net.IPv4                     as IPv4
import qualified Network.Simple.TCP           as TCP

import           Net.IPv4                     (IPv4 (..))
import           Network.Socket               (Socket)
import           Network.Socket.ByteString    (sendAll)

-- Conduit imports
import           Conduit                      (MonadThrow, mapM_C, printC,
                                               sinkNull)
import           Data.Conduit                 (ConduitT, runConduit, (.|))
import           Data.Conduit.Attoparsec      (conduitParser, sinkParser)
import           Data.Conduit.Network         (sourceSocket)

-- Library imports
import qualified Internal.Handshake           as Handshake
import qualified Internal.Message             as Message
import qualified Peer
import qualified Types

import           Internal.Handshake           (Handshake)
import           Internal.Message             (Message)
import           InternalMessage
import           Peer                         (Peer, PeerId)
import           Types                        (InfoHash)

type PeerM = ReaderT PeerEnv (LoggingT IO)
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
                      , torrentInfo         :: TorrentInfo
                      , peerId              :: !PeerId
                      , socket              :: !Socket
                      , ourPieces           :: !(TVar PieceSet)
                      , piecesMgrChan       :: !PiecesMgrChan
                      , peerAlive           :: !(TVar Bool)
                      , fromSelector        :: !(TChan SelectorToPeer)
                      , peerState           :: !(TVar PeerState)
                      , maybeRequestedPiece :: !(TVar (Maybe Int))
                      , pieces              :: !(TVar PieceSet)
                      , requestedBlocks     :: !(TVar PieceSet)
                      , blocks              :: !(TVar (Map Int BS.ByteString))
                      , nextBlocks          :: !(TVar (Maybe (Set Int)))
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
    where pmgrChan = (,) <$> newTChanIO <*> newTChanIO

run :: PeerM a -> PeerEnv -> IO a
run m conf = runStdoutLoggingT $ runReaderT m conf

start :: PeerEnv -> IO ()
start = run start'

start' :: PeerM ()
start' = do
    env <- ask
    hs <- sendHandshake >> recvHandshake
    asks infoHash >>= \ih -> unless (isValidHandshake ih hs) (error "Invalid handshake")
    startServices
    where startServices :: PeerM ()
          startServices = do
            env <- ask
            let asyncIO = liftIO . async . flip run env
            keepAlive  <- asyncIO keepAliveLoop
            checkAlive <- asyncIO checkAliveLoop
            inbound    <- asyncIO inboundLoop
            outbound   <- asyncIO outboundLoop
            void $ liftIO $ waitAnyCancel [keepAlive, checkAlive, inbound, outbound ]

mkPeerConnection :: (MonadIO m) => Peer -> m Peer
mkPeerConnection peer = do
    let peerAddr = IPv4.encodeString (Peer._ip peer)
        peerPort = show $ Peer._port peer
    socket <- liftIO $ fst <$> TCP.connectSock peerAddr peerPort
    let ap = Just $ Peer.newActivePeer socket
    return $ peer { Peer._maybeActive = ap }

sendHandshake :: (MonadReader PeerEnv m, MonadIO m) => m ()
sendHandshake = do
    env <- ask
    let ih           = infoHash env
        clientPeerId = peerId env
        hs = LBS.toStrict $ encode $ Handshake.new ih clientPeerId
    liftIO $ TCP.send (socket env) hs

recvHandshake :: (MonadReader PeerEnv m, MonadIO m, MonadThrow m) => m Handshake
recvHandshake = asks socket >>= \sock -> runConduit (sourceSocket sock .| sinkParser Handshake.parser)

inboundLoop :: PeerM ()
inboundLoop = asks socket >>= (\s -> forever $ runConduit $  sourceSocket s
                                                          .| conduitParser Message.parser
                                                          .| mapM_C (handleMessage . snd)
                                                          .| sinkNull)

outboundLoop :: (MonadReader env m, MonadIO m) => m ()
outboundLoop = todo --shouldRequest >>= \p -> forever (when (shouldRequest p) requestPiece)

requestPiece :: (MonadReader PeerEnv m, MonadIO m) => m ()
requestPiece =
    asks socket >>= \s -> do
                        (ix, offset, len) <- requestNextPiece
                        let req = LBS.toStrict $ encode $ Message.Request ix offset len
                        TCP.send s req

requestNextPiece :: (MonadReader env m, MonadIO m)
                 => m (Message.PieceIx, Message.PieceOffset, Message.PieceRequestLen)
requestNextPiece = do
    env <- ask
    --liftIO $ atomically $ do
    --    writeTChan (toPiecesMgr env) RequestNextPiece
    --    -- (NextPiece ix off len) <- readTChan (fromPiecesMgr conf)
    --    -- return (ix, off, len)
    todo

shouldRequest :: PeerM Bool
shouldRequest = todo

handleMessage :: Message -> PeerM ()
handleMessage msg = do
    logDebugN $ mconcat ["Handling message ", Text.pack (show msg)]
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
    TCP.send (socket env) (LBS.toStrict $ encode $ Message.Request pieceIx off len)

-- | Default block size in bytes
blockSize :: PeerM Int
blockSize = return (2 ^ 14) -- FIXME: If downloading last piece, this could be smaller

notifyPiecesMgr :: PeerToPiecesMgr -> PeerM ()
notifyPiecesMgr m = do
    (to, _) <- asks piecesMgrChan
    liftIO $ atomically $ writeTChan to m

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

-- Check if we should request new piece
-- if amInterested and not isChoking and we don't have a request
-- then pick a random piece from the pieces that remote has and we don't
-- and start downloading it
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

pickNewPiece :: PieceSet -> Int
pickNewPiece = head . Set.toList

-- Check if we should be interested in peer
-- we should be interested when remote has pieces that we don't have
-- we should send Interested message when amInterested changes
-- if we got all remote pieces check if we should send NotInterested
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

-- Clear requested piece and blocks
cancelRequested :: (MonadReader PeerEnv m, MonadIO m) => m ()
cancelRequested = do
    env <- ask
    liftIO $ atomically $ do
        writeTVar (maybeRequestedPiece env) Nothing
        writeTVar (requestedBlocks env) Set.empty

-- Add piece to pieces that we have
addPiece :: (MonadReader PeerEnv m, MonadIO m) => Int -> m ()
addPiece ix = asks pieces >>= \p -> liftIO $ atomically $ modifyTVar p (Set.insert ix)

isValidHandshake :: Types.InfoHash -> Handshake -> Bool
isValidHandshake ih h = h ^. Handshake.infoHash == ih -- TODO: Check peer id

sendMessage :: (MonadReader PeerEnv m, MonadIO m) => Message -> m ()
sendMessage msg = asks socket >>= send'
    where send' sock = liftIO $ sendAll sock $ LBS.toStrict $ encode msg

withPeerSocket :: (MonadIO m) => Peer -> (TCP.Socket -> m a) -> m a
withPeerSocket peer f = do
    let (Just ap) = Peer._maybeActive peer
    let socket = Peer._socket ap
    f socket

keepAliveLoop :: (MonadReader PeerEnv m, MonadIO m, MonadLogger m) => m ()
keepAliveLoop = forever $ liftIO (threadDelay timeout) >> sendMessage Message.KeepAlive
    where timeout = 1000000 * 60 * 2

checkAliveLoop :: (MonadReader PeerEnv m, MonadIO m) => m ()
checkAliveLoop = do
    env <- ask
    liftIO $ forever $ do
        let timeout = 1000000 * 60 * 2
        threadDelay timeout
        atomically $ do
            alive <- readTVar (peerAlive env)
            unless alive (error "Peer is not alive anymore")
            writeTVar (peerAlive env) False

--selectorListenerLoop :: (MonadReader PeerEnv m, MonadIO m) => m ()
--selectorListenerLoop = do
--    env <- ask
--    msg <- liftIO $ atomically $ readTChan (fromSelector env)
--    peer <- liftIO $ readPeer env
--    case msg of
--        SetInterest b -> liftIO $ atomically $ writeTVar (getPeer env) (Peer.setAmInterested peer b)

logProtocol t = traceM

todo = error "TODO"
