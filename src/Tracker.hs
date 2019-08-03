{-# LANGUAGE OverloadedStrings #-}

module Tracker
       ( peers
       , mkTrackerRequest
       , sendRequest
       , TrackerResponse
       )
       where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Map              as Map

import           Data.Binary.Get
import           Network.HTTP.Simple

import           Control.Monad         (replicateM)
import           Data.BEncode          (BEncode (..), bRead)
import           Data.List             (intercalate)
import           Network.Simple.TCP    (HostName, ServiceName)
import           Network.Socket        (PortNumber)

import           Types                 (Announce, InfoHash, PeerId)

data TrackerRequest = TrackerRequest
    { trAnnounce   :: Announce
    , trInfoHash   :: InfoHash
    , trPeerId     :: PeerId
    , trListenPort :: PortNumber
    , trUploaded   :: Int
    , trDownloaded :: Int
    , trLeft       :: Int
    } deriving(Show)

data TrackerResponse = TrackerResponse
    { interval :: Integer
    , peers    :: [(HostName, ServiceName)]
    } deriving(Show)

mkTrackerRequest :: Announce -> InfoHash -> PeerId -> PortNumber -> TrackerRequest
mkTrackerRequest announce infoHash peerId port = TrackerRequest
    { trAnnounce   = announce
    , trInfoHash   = infoHash
    , trPeerId     = peerId
    , trListenPort = port
    , trUploaded   = 0
    , trDownloaded = 0
    , trLeft       = 0
    }

sendRequest :: TrackerRequest -> IO (Either LBS.ByteString TrackerResponse)
sendRequest r = do
    request <- setRequestQueryString query <$> parseRequest (C.unpack $ trAnnounce r)
    parseTrackerResponse . getResponseBody <$> httpLBS request
    where
        query :: [(BS.ByteString, Maybe BS.ByteString)]
        query = let itobs = C.pack . show
                in map (\(x, y) -> (x, Just y))
                       [ ("info_hash",          trInfoHash r)
                       , ("peer_id",            trPeerId r)
                       , ("port",       itobs $ fromIntegral (trListenPort r))
                       , ("uploaded",   itobs $ trUploaded r)
                       , ("downloaded", itobs $ trDownloaded r)
                       , ("left",       itobs $ trLeft r)
                       , ("compact",    "1")
                       ]

parseTrackerResponse :: LBS.ByteString -> Either LBS.ByteString TrackerResponse
parseTrackerResponse bs =
    case failureReason of
        Nothing          -> Right $ TrackerResponse wait_interval peerList
        Just (BString s) -> Left s
        _                -> error "Invalid failure reason"
    where
        failureReason = Map.lookup "failure_reason" bdict
        Just (BInt wait_interval) = Map.lookup "interval" bdict
        Just peerList      = parseCompactPeerList <$> Map.lookup "peers" bdict
        Just (BDict bdict) = bRead bs

parseCompactPeerList :: BEncode -> [(HostName, ServiceName)]
parseCompactPeerList (BString s)
    | LBS.null s = []
    | otherwise  = runGet getCompactPeer (LBS.take 6 s) : parseCompactPeerList (BString $ LBS.drop 6 s)
parseCompactPeerList _ = error "Invalid argument"

getCompactPeer :: Get (HostName, ServiceName)
getCompactPeer =
    let getHost = intercalate "." <$> replicateM 4 (show <$> getWord8)
        getPort = show <$> getWord16be
    in (,) <$> getHost <*> getPort
