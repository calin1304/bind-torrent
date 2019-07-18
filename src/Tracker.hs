{-# LANGUAGE OverloadedStrings #-}

module Tracker where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Map              as Map

import           Data.Binary.Get
import           Net.IPv4
import           Network.HTTP.Simple

import           Control.Monad         (replicateM)
import           Data.BEncode          (BEncode (..), bRead)
import           Data.List             (intercalate)
import           Network.Simple.TCP    (HostName, ServiceName)
import           Network.Socket        (PortNumber, SockAddr)
import           Numeric               (showInt)

import           Types                 (Announce, InfoHash, PeerId)

data TrackerRequest = TrackerRequest
    { announce   :: Announce
    , infoHash   :: InfoHash
    , peerId     :: PeerId
    , listenPort :: PortNumber
    , uploaded   :: Int
    , downloaded :: Int
    , left       :: Int
    } deriving(Show)

data TrackerResponse = TrackerResponse
    { interval :: Integer
    , peers    :: [(HostName, ServiceName)]
    } deriving(Show)

mkTrackerRequest :: Announce -> InfoHash -> PeerId -> PortNumber -> TrackerRequest
mkTrackerRequest an ih pi port = TrackerRequest
                               { announce = an
                               , infoHash = ih
                               , peerId = pi
                               , listenPort = port
                               , uploaded = 0
                               , downloaded = 0
                               , left = 0
                               }

sendRequest :: TrackerRequest -> IO (Either LBS.ByteString TrackerResponse)
sendRequest r = do
    request <- setRequestQueryString query <$> parseRequest (C.unpack $ announce r)
    parseTrackerResponse . getResponseBody <$> httpLBS request
    where
        query :: [(BS.ByteString, Maybe BS.ByteString)]
        query = let itobs = C.pack . show
                in map (\(x, y) -> (x, Just y))
                       [ ("info_hash",          infoHash r)
                       , ("peer_id",            peerId r)
                       , ("port",       itobs $ fromIntegral (listenPort r))
                       , ("uploaded",   itobs $ uploaded r)
                       , ("downloaded", itobs $ downloaded r)
                       , ("left",       itobs $ left r)
                       , ("compact",    "1")
                       ]

parseTrackerResponse :: LBS.ByteString -> Either LBS.ByteString TrackerResponse
parseTrackerResponse bs =
    case failureReason of
        Nothing          -> Right $ TrackerResponse wait_interval peerList
        Just (BString s) -> Left s
    where
        failureReason = Map.lookup "failure_reason" bdict
        Just (BInt wait_interval) = Map.lookup "interval" bdict
        Just peerList      = parseCompactPeerList <$> Map.lookup "peers" bdict
        Just (BDict bdict) =  bRead bs

parseCompactPeerList :: BEncode -> [(HostName, ServiceName)]
parseCompactPeerList (BString s)
    | LBS.null s = []
    | otherwise  = runGet getCompactPeer (LBS.take 6 s) : parseCompactPeerList (BString $ LBS.drop 6 s)

getCompactPeer :: Get (HostName, ServiceName)
getCompactPeer =
    let getHost = intercalate "." <$> replicateM 4 (show <$> getWord8)
        getPort = show . fromIntegral <$> getWord16be
    in (,) <$> getHost <*> getPort
