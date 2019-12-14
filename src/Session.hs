{-# LANGUAGE FlexibleContexts  #-}

module Session
       ( SessionEnv
       , newEnvFromMeta
       , start
       , sessionTorrent
       , sessionInfoHash
       , TorrentStatus(..)
       ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.Reader
import           Data.BEncode
import           Data.Either
import           Data.Torrent
import           Debug.Trace

import           Control.Concurrent           (threadDelay)
import           Control.Monad                (forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.STM            (atomically)
import           Crypto.Hash.SHA1             (hashlazy)
import           Data.Function                ((&))
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe)
import           Data.Set                     (Set)
import           Network.Simple.TCP           (HostName, ServiceName, closeSock,
                                               connectSock)
import           Network.Socket               (PortNumber, SockAddr, Socket)

import           InternalMessage              (PiecesMgrMessage (..),
                                               SessionMessage (..))
import           MovingWindow                 (MovingWindow)
import           TorrentInfo
import           Types                        (InfoHash, PeerId)

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set

import qualified MovingWindow                 as MW
import qualified Peer
import qualified PiecesMgr
import qualified Tracker


type Peer = (HostName, ServiceName)

type SessionM a = ReaderT SessionEnv IO a

data SessionEnv = SessionEnv
              { seInfoHash             :: InfoHash
              , seTorrent              :: Torrent
              , seListenPort           :: PortNumber
              , seTorrentStatus        :: TVar (Maybe TorrentStatus)
              , seToSession            :: TChan SessionMessage
              , sePeerId               :: PeerId
              , seDownloadedPieces     :: TVar (Set Int)
              , seDownloadMovingWindow :: TVar MovingWindow
              , seToPiecesMgr          :: TChan PiecesMgrMessage
              }

data SessionCanceled = SessionCanceled deriving (Show)

instance Exception SessionCanceled

sessionTorrent :: SessionEnv -> Torrent
sessionTorrent = seTorrent

sessionInfoHash :: SessionEnv -> InfoHash
sessionInfoHash = seInfoHash

newEnvFromMeta :: LBS.ByteString -> TVar (Maybe TorrentStatus) -> TChan SessionMessage -> IO SessionEnv
newEnvFromMeta meta ts chan =
    SessionEnv infoHash torrent listenPort ts chan
        <$> randomPeerId
        <*> newTVarIO Set.empty
        <*> newTVarIO (MW.new 4)
        <*> newTChanIO
    where
        metaDict     = fromMaybe (error "Could not decode meta file") (bRead meta)
        infoHash     = bencodeHash $ fromJust $ lookupBDict "info" metaDict
        bencodeHash  = hashlazy . bPack
        randomPeerId = return "01234567890123456789" :: IO PeerId
                       -- ^ FIXME: Remove hardcoded value
        listenPort   = 6881
        torrent      = fromRight (error "Error reading torrent") $ readTorrent meta

lookupBDict :: String -> BEncode -> Maybe BEncode
lookupBDict k (BDict d) = Map.lookup k d
lookupBDict _ _         = error "Invalid argument"

torrentStatusLoop :: SessionEnv -> IO ()
torrentStatusLoop env = do
    liftIO $ do
        atomically $ do
            downloadSpeed' <- fromIntegral . MW.get <$> readTVar (seDownloadMovingWindow env)
            downloaded <- toRational . Set.size <$> readTVar (seDownloadedPieces env)
            let len = tLength $ tInfo $ seTorrent env
            let totalPieces = toRational $ len `div` tPieceLength (tInfo $ seTorrent env)
            modifyTVar' (seTorrentStatus env)
                (updateStatus downloadSpeed' (min (round (downloaded / totalPieces * 100)) 100))
        threadDelay 1000000
    torrentStatusLoop env
    where
        updateStatus dlSpeed dled _ = Just $ TorrentStatus dled dlSpeed

messageListener :: TChan SessionMessage -> IO ()
messageListener chan = do
    msg <- atomically $ readTChan chan
    case msg of
        Cancel -> void $ throw SessionCanceled
    messageListener chan

start :: SessionEnv -> IO ()
start env = void $ async $ runReaderT start' env
    where
        start' = do
            logSession "Starting session"
            peers <- take 20 . filter (\(_, port) -> port /= "6881") <$> getPeers
            -- ^ FIXME: getPeers also returns us
            mconcat ["Got peers: ", show peers] & logSession
            socks <- liftIO $ catMaybes <$> forM peers connectToPeer
            tsLoop <- liftIO $ async $ torrentStatusLoop env
            listener <- liftIO $ async $ messageListener (seToSession env)
            piecesMgr <- liftIO $ async startPiecesMgr
            liftIO $ do
                mapConcurrently_ startPeer socks
                -- ^ FIXME: Threads don't end when session ends
                void $ waitAnyCatchCancel [tsLoop, listener, piecesMgr]
            forM_ socks closeSock
            logSession "Session end"
            where startPeer :: Socket -> IO ()
                  startPeer sock = do
                      let ih    = seInfoHash env
                          ti    = (tInfo . seTorrent) env
                          pid   = sePeerId env
                          ourPs = seDownloadedPieces env
                          mw    = seDownloadMovingWindow env
                          toPiecesMgr = seToPiecesMgr env
                      peerEnv <- liftIO $ Peer.newConfig ih ti pid sock ourPs mw toPiecesMgr
                      Peer.start peerEnv

                  startPiecesMgr :: IO ()
                  startPiecesMgr = do
                      let piecesMgrEnv = PiecesMgr.newEnvFromMeta (seTorrent env) (seToPiecesMgr env)
                      bracket piecesMgrEnv PiecesMgr.cleanup PiecesMgr.start

                  connectToPeer :: (HostName, ServiceName) -> IO (Maybe Socket)
                  connectToPeer (host, port) = do
                      result <- try (connectSock host port) :: IO (Either SomeException (Socket, SockAddr))
                      case result of
                          Right (sock, _) -> return $ Just sock
                          Left _          -> return Nothing

getPeers :: SessionM [Peer]
getPeers = do
    Just announce <- fmap LBS.toStrict <$> asks (tAnnounce . seTorrent)
    ih <- asks seInfoHash
    peerId <- asks sePeerId
    lp <- asks seListenPort
    response <- liftIO $
        try (Tracker.sendRequest $ Tracker.mkTrackerRequest announce ih peerId lp)
            :: SessionM (Either SomeException (Either LBS.ByteString Tracker.TrackerResponse))
    case response of
        Left _  -> return []
        Right x ->
            case x of
                Left _  -> return []
                Right p -> return $ Tracker.peers p

logSession :: (Applicative m) => String -> m ()
logSession s = traceM $ mconcat ["Session: ", s]
