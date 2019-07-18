{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

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
import           Data.Function                ((&))
import           Data.Maybe                   (fromJust)
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import           Network.Simple.TCP           (HostName, ServiceName,
                                               connectSock)
import           Network.Socket               (PortNumber, SockAddr, Socket)

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
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
              , torrent          :: !Torrent
              , listenPort       :: !PortNumber
              , defaultMaxConns  :: !Int
              , peerId           :: !PeerId
              , downloadedPieces :: !(TVar (Set Int))
              }

run :: SessionM a -> SessionEnv -> IO a
run r s = runStdoutLoggingT $ runReaderT r s

newEnvFromMeta :: LBS.ByteString -> IO SessionEnv
newEnvFromMeta meta = SessionEnv ih torrent listenPort 4 <$> randomPeerId <*> newTVarIO Set.empty
    where Just (BDict metaDict) = bRead meta
          ih = bencodeHash $ fromJust $ Map.lookup "info" metaDict
          bencodeHash = hashlazy . bPack
          randomPeerId = return "01234567890123456789" :: IO PeerId -- FIXME: Remove hardcoded value
          listenPort = 6881
          torrent = fromRight (error "Error reading torrent") $ readTorrent meta

start :: SessionEnv -> IO ()
start = run $ do
    env <- ask
    -- FIXME: getPeers also returns us
    peers <- filter (\(_, port) -> port /= "6881") <$> getPeers
    mconcat ["Got peers: ", show peers] & logSession
    socks <- forM peers $ fmap fst . uncurry connectSock
    peers' <- forM socks startPeer
    void $ liftIO $ waitAnyCancel peers'
    where startPeer :: Socket -> SessionM (Async ())
          startPeer sock = do
              ih <- asks infoHash
              ti <- asks $ tInfo . torrent
              pid <- asks peerId
              ourPs <- asks downloadedPieces
              env <- liftIO $ Peer.newConfig ih ti pid sock ourPs
              liftIO $ async $ Peer.start env

getPeers :: SessionM [(HostName, ServiceName)]
getPeers = do
    Just announce <- fmap LBS.toStrict <$> asks (tAnnounce . torrent)
    ih <- asks infoHash
    pi <- asks peerId
    lp <- asks listenPort
    response <- liftIO $ Tracker.sendRequest $ Tracker.mkTrackerRequest announce ih pi lp
    case response of
        Left _  -> error "Response error"
        Right x -> return $ Tracker.peers x

logSession s = traceM $ mconcat ["Session: ", s]
