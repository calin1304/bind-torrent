{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Session
       ( SessionEnv
       , newEnvFromMeta
       , start
       ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad               (forM_)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Crypto.Hash.SHA1            (hashlazy)
import           Data.BEncode
import           Data.Either
import           Data.Function               ((&))
import           Data.Maybe                  (catMaybes, fromJust, fromMaybe)
import           Data.Set                    (Set)
import           Data.Torrent
import           Debug.Trace
import           Network.Simple.TCP          (HostName, ServiceName, closeSock,
                                              connectSock)
import           Network.Socket              (PortNumber, SockAddr, Socket)

import           Types                       (InfoHash, PeerId)

import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set

import qualified Peer
import qualified Tracker

type SessionM a = ReaderT SessionEnv (LoggingT IO) a

data SessionEnv = SessionEnv
              { seInfoHash         :: !InfoHash
              , seTorrent          :: !Torrent
              , seListenPort       :: !PortNumber
              , sePeerId           :: !PeerId
              , seDownloadedPieces :: !(TVar (Set Int))
              }

run :: SessionM a -> SessionEnv -> IO a
run r s = runStdoutLoggingT $ runReaderT r s

newEnvFromMeta :: LBS.ByteString -> IO SessionEnv
newEnvFromMeta meta = SessionEnv infoHash torrent listenPort <$> randomPeerId <*> newTVarIO Set.empty
    where
        metaDict     = fromMaybe (error "Could not decode meta file") (bRead meta)
        infoHash     = bencodeHash $ fromJust $ lookupBDict "info" metaDict
        bencodeHash  = hashlazy . bPack
        randomPeerId = return "01234567890123456789" :: IO PeerId -- FIXME: Remove hardcoded value
        listenPort   = 6881
        torrent      = fromRight (error "Error reading torrent") $ readTorrent meta

lookupBDict :: String -> BEncode -> Maybe BEncode
lookupBDict k (BDict d) = Map.lookup k d
lookupBDict _ _         = error "Invalid argument"

start :: SessionEnv -> IO ()
start = run $ do
    env <- ask
    -- FIXME: getPeers also returns us
    peers <- take 20 . filter (\(_, port) -> port /= "6881") <$> getPeers
    mconcat ["Got peers: ", show peers] & logSession
    socks <- liftIO $ catMaybes <$> forM peers connectToPeer
    -- peers' <- forM socks startPeer
    liftIO $ mapConcurrently_ (startPeer env) socks
    forM_ socks closeSock
    where startPeer :: SessionEnv -> Socket -> IO ()
          startPeer env sock = do
              let ih    = seInfoHash env
                  ti    = (tInfo . seTorrent) env
                  pid   = sePeerId env
                  ourPs = seDownloadedPieces env
              peerEnv <- liftIO $ Peer.newConfig ih ti pid sock ourPs
              Peer.start peerEnv

          connectToPeer :: (HostName, ServiceName) -> IO (Maybe Socket)
          connectToPeer (host, port) = do
              result <- try (connectSock host port) :: IO (Either SomeException (Socket, SockAddr))
              case result of
                  Right (sock, _) -> return $ Just sock
                  Left _          -> return Nothing

getPeers :: SessionM [(HostName, ServiceName)]
getPeers = do
    Just announce <- fmap LBS.toStrict <$> asks (tAnnounce . seTorrent)
    ih <- asks seInfoHash
    peerId <- asks sePeerId
    lp <- asks seListenPort
    response <- liftIO $ Tracker.sendRequest $ Tracker.mkTrackerRequest announce ih peerId lp
    case response of
        Left _  -> error "Response error"
        Right x -> return $ Tracker.peers x

logSession :: (Applicative m) => String -> m ()
logSession s = traceM $ mconcat ["Session: ", s]
