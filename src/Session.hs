{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Session where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Text                    as Text
import qualified Net.IPv4                     as IPv4

import           Control.Concurrent.STM.TVar
import           Control.Monad                (forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Crypto.Hash.SHA1             (hashlazy)
import           Data.Maybe                   (fromJust)
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import           Network.Simple.TCP           (HostName, ServiceName,
                                               connectSock)
import           Network.Socket               (PortNumber, SockAddr, Socket)

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.BEncode
import           Data.Either
import           Data.Torrent
import           Debug.Trace
import           Path

-- Library imports
import           Types                        (Announce, InfoHash, PeerId)

import qualified Peer
import qualified PiecesMgr
import qualified Tracker

type SessionM a = ReaderT SessionEnv (LoggingT IO) a

data SessionEnv = SessionEnv
              { infoHash         :: !InfoHash
              , announce         :: !Announce
              , listenPort       :: !PortNumber
              , tinfo            :: !TorrentInfo
              , defaultMaxConns  :: !Int
              , peerId           :: !PeerId
              , downloadedPieces :: !(TVar (Set Int))
              }

run :: SessionM a -> SessionEnv -> IO a
run r s = runStdoutLoggingT $ runReaderT r s

newEnvFromMeta :: LBS.ByteString -> IO SessionEnv
newEnvFromMeta meta = SessionEnv ih announce listenPort ti 4 <$> randomPeerId <*> newTVarIO Set.empty
    where Just (BDict torrent) = bRead meta
          ih = bencodeHash $ fromJust $ Map.lookup "info" torrent
          announce = let (Just (BString s)) = Map.lookup "announce" torrent in LBS.toStrict s
          bencodeHash = hashlazy . bPack
          randomPeerId = return "01234567890123456789" -- FIXME
          listenPort = 6881
          ti = tInfo $ fromRight (error "Error reading torrent") $ readTorrent meta

start :: SessionEnv -> IO ()
start = run $ do
    env <- ask
    services <- startServices
    -- FIXME: getPeers also returns us
    peers <- filter (\(_, port) -> port /= "6881") <$> getPeers
    socks <- forM peers $ fmap fst . uncurry connectSock
    peers' <- forM socks startPeer
    void $ liftIO $ waitAnyCancel peers'
    where startServices :: SessionM [Async ()]
          startServices = sequence [ startPiecesMgr ]

          startPiecesMgr :: SessionM (Async ())
          startPiecesMgr = do
            ti <- asks tinfo
            let root = $(mkAbsDir "/home/calin/Downloads")
                pieceLen = 2 ^ 15
            liftIO $ async $ PiecesMgr.start =<< PiecesMgr.newEnvFromInfo ti root pieceLen

          startPeer :: Socket -> SessionM (Async ())
          startPeer sock = do
              ih <- asks infoHash
              ti <- asks tinfo
              pid <- asks peerId
              ourPs <- asks downloadedPieces
              env <- liftIO $ Peer.newConfig ih ti pid sock ourPs
              liftIO $ async $ Peer.start env


getPeers :: (MonadReader SessionEnv m, MonadIO m ) => m [(HostName, ServiceName)]
getPeers = do
    config <- ask
    response <- liftIO $ Tracker.sendRequest $ Tracker.mkTrackerRequest (announce config)
                                                  (infoHash config)
                                                  (peerId config)
                                                  (listenPort config)
    case response of
        Left _  -> error "Response error"
        Right x -> return $ x ^. Tracker.peers

