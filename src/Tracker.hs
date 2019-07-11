{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tracker where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as LBS

import           Control.Lens
import           Data.Binary.Get
import           Net.IPv4
import           Network.HTTP.Simple

import           Data.BEncode          (BEncode (..), bRead)
import           Network.Socket        (PortNumber, SockAddr)
import           Numeric               (showInt)

-- Lib imports
import qualified BEncoding

import           Types                 (Announce, InfoHash, PeerId)

data TrackerRequest = TrackerRequest
    { _announce   :: Announce
    , _infoHash   :: InfoHash
    , _peerId     :: PeerId
    , _listenPort :: PortNumber
    , _uploaded   :: Int
    , _downloaded :: Int
    , _left       :: Int
    } deriving(Show)
makeLenses ''TrackerRequest

data TrackerResponse = TrackerResponse
    { _interval :: Integer
    , _peers    :: [SockAddr]
    } deriving(Show)
makeLenses ''TrackerResponse

mkTrackerRequest :: Announce -> InfoHash -> PeerId -> PortNumber -> TrackerRequest
mkTrackerRequest an ih pi port = TrackerRequest
                               { _announce = an
                               , _infoHash = ih
                               , _peerId = pi
                               , _listenPort = port
                               , _uploaded = 0
                               , _downloaded = 0
                               , _left = 0
                               }

sendRequest :: TrackerRequest -> IO (Either LBS.ByteString TrackerResponse)
sendRequest r = do
    request <- setRequestQueryString query <$> parseRequest (C.unpack $ r ^. announce)
    parseTrackerResponse . getResponseBody <$> httpLBS request
    where
        query :: [(BS.ByteString, Maybe BS.ByteString)]
        query = let itobs = C.pack . show
                in map (\(x, y) -> (x, Just y))
                       [ ("info_hash",          r ^. infoHash)
                       , ("peer_id",            r ^. peerId)
                       , ("port",       itobs $ fromIntegral (r ^. listenPort))
                       , ("uploaded",   itobs $ r ^. uploaded)
                       , ("downloaded", itobs $ r ^. downloaded)
                       , ("left",       itobs $ r ^. left)
                       , ("compact",    "1")
                       ]

parseTrackerResponse :: LBS.ByteString -> Either LBS.ByteString TrackerResponse
parseTrackerResponse bs =
    case failureReason of
        Nothing          -> Right $ TrackerResponse wait_interval peerList
        Just (BString s) -> Left s
    where
        failureReason = BEncoding.lookupBDict "failure_reason" bdict :: Maybe BEncode
        wait_interval = let Just (BInt x) = BEncoding.lookupBDict "interval" bdict in x
        peerList      = let Just x = parseCompactPeerList <$> BEncoding.lookupBDict "peers" bdict
                         in x
        bdict         = let Just x = bRead bs in x

parseCompactPeerList :: BEncode -> [SockAddr]
parseCompactPeerList (BString s)
    | LBS.null s = []
    | otherwise  = runGet getCompactPeer (LBS.take 6 s) : parseCompactPeerList (BString $ LBS.drop 6 s)

getCompactPeer :: Get SockAddr
getCompactPeer = undefined
    -- let getHost = ipv4 <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
    --     getPort = fromIntegral <$> getWord16be
    -- in Peer.new <$> getHost <*> getPort
