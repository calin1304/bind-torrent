{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Session where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text                    as Text

import           Control.Monad                (forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Crypto.Hash.SHA1             (hashlazy)
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)
import           Network.Socket               (PortNumber, SockAddr)

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

import qualified BEncoding
import qualified Peer
import qualified PiecesMgr
import qualified Tracker

type SessionM a = ReaderT SessionEnv (LoggingT IO) a

data SessionEnv = SessionEnv
              { infoHash        :: InfoHash
              , announce        :: Announce
              , listenPort      :: PortNumber
              , tinfo           :: TorrentInfo
              , defaultMaxConns :: Int
              , peerId          :: PeerId
              }

run :: SessionM a -> SessionEnv -> IO a
run r s = runStdoutLoggingT $ runReaderT r s

newEnvFromMeta :: LBS.ByteString -> IO SessionEnv
newEnvFromMeta meta = SessionEnv ih announce listenPort ti 4 <$> randomPeerId
    where Just torrent = bRead meta
          ih = bencodeHash $ fromJust $ BEncoding.lookupBDict "info" torrent
          announce = LBS.toStrict $ (\(BString s) -> s) $ fromJust
                        $ BEncoding.lookupBDict "announce" torrent
          bencodeHash = hashlazy . bPack
          randomPeerId = return "01234567890123456789" -- FIXME
          listenPort = 6881
          ti = tInfo $ fromRight (error "Error reading torrent") $ readTorrent meta

start :: SessionEnv -> IO ()
start = run start'

start' :: SessionM ()
start' = do
    traceM "Starting services"
    env <- ask
    services <- startServices
    void $ liftIO $ waitAnyCancel services
    where startServices :: SessionM [Async ()]
          startServices = sequence [ startPiecesMgr ]

          startPiecesMgr :: SessionM (Async ())
          startPiecesMgr = do
            traceM "Starting pieces manager"
            ti <- asks tinfo
            let root = $(mkAbsDir "/home/calin/Downloads")
                pieceLen = 2 ^ 15
            liftIO $ async $ PiecesMgr.start =<< PiecesMgr.newEnvFromInfo ti root pieceLen

          -- startSelector :: SessionM (Async ())
          -- startSelector = do
          --   traceM "Starting selector"
          --   maxConn <- asks defaultMaxConns
          --   e <- liftIO $ Selector.newEnv [] maxConn
          --   liftIO $ async $ Selector.start e

getPeers :: (MonadReader SessionEnv m, MonadIO m ) => m [SockAddr]
getPeers = do
    config <- ask
    traceM $ mconcat ["Requesting peers from ", show (announce config)]
    response <- liftIO $ Tracker.sendRequest $ Tracker.mkTrackerRequest (announce config)
                                                  (infoHash config)
                                                  (peerId config)
                                                  (listenPort config)
    case response of
        Left _  -> error "Response error"
        Right x -> return $ x ^. Tracker.peers

