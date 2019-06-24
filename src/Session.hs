{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Session where

import           Control.Lens
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)
import           Crypto.Hash.SHA1       (hashlazy)
import           Data.BEncode
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Maybe             (fromJust)
import           Data.Torrent
import           Network.Socket         (PortNumber)
import qualified Data.Text as Text

import           Peer                   (Peer, PeerId)
import           Types                  (Announce, InfoHash)

import qualified BEncoding              as BEncoding
import qualified PiecesManager          as PiecesManager
import qualified Tracker as Tracker
import qualified Protocol as Protocol 
import qualified Peer as Peer

type SessionM a = ReaderT Config (LoggingT IO) a

run :: SessionM a -> Config -> IO a
run r s = runStdoutLoggingT $ runReaderT r s

data Config = Config
              { infoHash          :: InfoHash
              , announce          :: Announce
              , peerId :: PeerId
              , listenPort :: PortNumber
              }

newConfigFromMeta :: LBS.ByteString -> IO Config
newConfigFromMeta meta = Config ih announce <$> randomPeerId <*> pure listenPort
    where Just torrent = bRead meta 
          ih = bencodeHash $ fromJust $ BEncoding.lookupBDict "info" torrent 
          announce = LBS.toStrict $ (\(BString s) -> s) $ fromJust 
                        $ BEncoding.lookupBDict "announce" torrent

          bencodeHash :: BEncode -> InfoHash
          bencodeHash = hashlazy . bPack

          randomPeerId :: IO PeerId
          randomPeerId = return "01234567890123456789" -- FIXME

          listenPort :: PortNumber
          listenPort = 6881
          
new :: SessionM ()
new = do
    config <- ask 
    peers <- getPeers
    let peers' = filter (\p -> Peer._port p /= listenPort config) peers
    $(logDebugSH) peers'
    let ih = infoHash config
    let pi = peerId config
    liftIO $ Protocol.run (Protocol.new (head peers')) (Protocol.newConfig ih pi)


getPeers :: SessionM [Peer]
getPeers = do
    config <- ask
    $(logDebug) (Text.concat ["Requesting peers from ", Text.pack (show (announce config))])
    Right response <- liftIO $ Tracker.sendRequest
                       $ Tracker.mkTrackerRequest (announce config)
                                                  (infoHash config)
                                                  (peerId config)
                                                  (listenPort config)
    return $ response ^. Tracker.peers
