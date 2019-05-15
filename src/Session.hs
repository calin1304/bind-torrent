{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Session where

import           Control.Lens
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
-- import           Control.Monad.State.Lazy (StateT, evalStateT, get)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Crypto.Hash.SHA1       (hashlazy)
import           Data.BEncode
import           Data.ByteString        as BS
import           Data.ByteString.Lazy   as LBS
import           Data.Maybe             (fromJust)
import           Data.Torrent

import           BEncoding              (lookupBDict)
import           Peer                   (Peer, PeerId)
import qualified Tracker
import           Types                  (Announce, InfoHash)

type Session = ReaderT SessionState IO
data SessionState = SessionState
                  { _peerId     :: PeerId
                  , _listenPort :: Int
                  , _infoHash   :: InfoHash
                  }
makeLenses ''SessionState

runSessionFromFile :: String -> IO ()
{- TODO:
    - Initialize from config file
    - Read and parse file
    - Ask tracker for peers
    - Connect to peers and start download
-}
runSessionFromFile filename = do
    Just torrent <- bRead <$> LBS.readFile filename
    peerId <- randomPeerId -- TODO: Get from config file
    let sessionState = SessionState
                     { _peerId = peerId
                     , _listenPort = 6881 -- TODO: Try another one if port is busy
                     , _infoHash = bencodeHash $ fromJust
                                 $ lookupBDict "info" torrent
                     }
    let announce = LBS.toStrict $ (\(BString s) -> s) $ fromJust
                 $ lookupBDict "announce" torrent
    peers <- runReaderT (getPeers announce) sessionState
    forM_ peers print

    where
        bencodeHash :: BEncode -> InfoHash
        bencodeHash = hashlazy . bPack

        randomPeerId :: IO PeerId
        randomPeerId = return $ "01234567890123456789"

getPeers :: Announce -> Session [Peer]
getPeers announce = do
    s <- ask
    Right response <- liftIO $ Tracker.sendRequest
                   $ Tracker.mkTrackerRequest announce
                                              (s ^. infoHash)
                                              (s ^. peerId)
                                              (s ^. listenPort)
    return $ response ^. Tracker.peers