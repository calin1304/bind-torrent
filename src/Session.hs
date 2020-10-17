{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Session
       ( SessionEnv
       , newEnvFromMeta
       , start
       , sessionTorrent
       , sessionInfoHash
       , TorrentStatus(..)
       ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TBChan
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Data.BEncode
import           Data.Either
import           Data.Torrent
import           Debug.Trace

import           Control.Concurrent            (threadDelay)
import           Control.Monad                 (forM_)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.STM             (atomically)
import           Crypto.Hash.SHA1              (hashlazy)
import           Data.BEncode.Parser           (dict, runParser)
import           Data.Function                 ((&))
import           Data.Maybe                    (catMaybes, fromMaybe)
import           Data.Set                      (Set)
import           Network.Simple.TCP            (HostName, ServiceName,
                                                closeSock, connectSock)
import           Network.Socket                (SockAddr, Socket)

import           Config
import           InternalMessage               (PiecesMgrMessage (..),
                                                SessionMessage (..))
import           MovingWindow                  (MovingWindow)
import           TorrentInfo
import           Types                         (InfoHash, PeerId)

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LC
import qualified Data.Set                      as Set

import qualified MovingWindow                  as MW
import qualified Peer
import qualified PiecesMgr
import qualified Tracker


type Peer = (HostName, ServiceName)

type SessionM a = ReaderT SessionEnv IO a

data SessionEnv = SessionEnv
              { seInfoHash             :: InfoHash
              , seTorrent              :: Torrent
              , _settings              :: Config
              , seTorrentStatus        :: TVar (Maybe TorrentStatus)
              , seToSession            :: TBChan SessionMessage
              , sePeerId               :: PeerId
              , seDownloadedPieces     :: TVar (Set Int)
              , seDownloadMovingWindow :: TVar MovingWindow
              , seToPiecesMgr          :: TBChan PiecesMgrMessage
              }
makeLenses ''SessionEnv

data SessionCanceled = SessionCanceled deriving (Show)

instance Exception SessionCanceled

sessionTorrent :: SessionEnv -> Torrent
sessionTorrent = seTorrent

sessionInfoHash :: SessionEnv -> InfoHash
sessionInfoHash = seInfoHash

newEnvFromMeta
    :: FilePath
    -> Config
    -> TVar (Maybe TorrentStatus)
    -> TBChan SessionMessage
    -> IO SessionEnv
newEnvFromMeta meta config ts chan =
    SessionEnv infoHash torrent config ts chan
        <$> randomPeerId
        <*> newTVarIO Set.empty
        <*> newTVarIO (MW.new 4)
        <*> newTBChanIO (config ^. clientConfig . chanCapacity . to fromIntegral)
    where
        metaDict     =
            fromMaybe (error "Could not decode meta file") (bRead (LC.pack meta))
        infoHash     = fromRight (error "Could not decode info hash")
            $ bencodeHash <$> runParser (dict "info") metaDict
        bencodeHash  = hashlazy . bPack
        randomPeerId = return "01234567890123456789" :: IO PeerId
                       -- ^ FIXME: Remove hardcoded value
        torrent      =
            fromRight (error "Error reading torrent") $ readTorrent (LC.pack meta)

torrentStatusLoop :: SessionEnv -> IO ()
torrentStatusLoop env = do
    liftIO $ do
        atomically $ do
            dlSpeed <- fromIntegral . fromMaybe 0 . MW.get
                <$> readTVar (seDownloadMovingWindow env)
            downloaded <- toRational . Set.size
                <$> readTVar (seDownloadedPieces env)
            let len = tLength $ tInfo $ seTorrent env
            let totalPieces = toRational $ len `div` tPieceLength (tInfo $ seTorrent env)
            modifyTVar' (seTorrentStatus env)
                (updateStatus dlSpeed (min (round (downloaded / totalPieces * 100)) 100))
        threadDelay 1000000
    torrentStatusLoop env
    where
        updateStatus dlSpeed dled _ = Just $ TorrentStatus dled dlSpeed

messageListener :: TBChan SessionMessage -> IO ()
messageListener chan = do
    atomically $ readTBChan chan >>= \case
        Cancel -> void $ throw SessionCanceled
    messageListener chan

start :: SessionEnv -> IO ()
start env = void $ async $ runReaderT start' env
    where
        start' = do
            logSession "Starting session"
            peers <- take 20 . filter (\(_, port) -> port /= "6881") <$> getPeers
            -- ^ FIXME: getPeers also returns us
            "Got peers: " <> show peers & logSession
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
                          bs = env ^. settings . clientConfig . blockSize
                      peerEnv <- liftIO $ Peer.newConfig ih ti pid sock ourPs mw toPiecesMgr (fromIntegral bs)
                      Peer.start peerEnv

                  startPiecesMgr :: IO ()
                  startPiecesMgr = do
                      let piecesMgrEnv = PiecesMgr.newEnvFromMeta (seTorrent env) (seToPiecesMgr env)
                      bracket piecesMgrEnv PiecesMgr.cleanup PiecesMgr.start

                  connectToPeer :: (HostName, ServiceName) -> IO (Maybe Socket)
                  connectToPeer (host, port) = do
                      result <- try (connectSock host port) :: IO (Either SomeException (Socket, SockAddr))
                      pure $ result ^? _Right . _1

getPeers :: SessionM [Peer]
getPeers = do
    Just announce <- fmap LBS.toStrict <$> asks (tAnnounce . seTorrent)
    ih <- asks seInfoHash
    peerId <- asks sePeerId
    lp <- views (settings . clientConfig . listeningPort) fromIntegral
    response <- liftIO $
        try (Tracker.sendRequest $ Tracker.mkTrackerRequest announce ih peerId lp)
            :: SessionM (Either SomeException (Either String Tracker.TrackerResponse))
    pure $ response ^? (_Right . _Right) & maybe [] Tracker.peers

logSession :: (Applicative m) => String -> m ()
logSession s = traceM $ "Session: " <> s
