{-# LANGUAGE OverloadedStrings #-}

module Tracker 
    ( 
      TrackerRequest
    , TrackerResponse
    , interval
    , peers
    , newTrackerRequest
    , makeTrackerRequest
    , InfoHash
    ) where


import Numeric(showInt)
import Data.Maybe(fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C

import Network.HTTP.Simple
import Data.BEncode(BEncode(..), bRead)
import Data.IP(fromHostAddress)
import Data.Binary.Get

import BEncoding
import Peer(Peer, PeerId, mkPeer)

type InfoHash = B.ByteString

data TrackerRequest = TrackerRequest 
    { announce   :: String
    , infoHash   :: InfoHash
    , peerId     :: PeerId
    , listenPort :: Int
    , uploaded   :: Int
    , downloaded :: Int
    , left       :: Int
    } deriving(Show)

data TrackerResponse = TrackerResponse
    { interval :: Integer
    , peers    :: [Peer]
    } deriving(Show)

makeOptions :: TrackerRequest -> [(B.ByteString, Maybe B.ByteString)]
makeOptions r = [ ("info_hash",  Just $ infoHash r)
                , ("peer_id",    Just $ peerId r)
                , ("port",       Just $ itobs $ listenPort r)
                , ("uploaded",   Just $ itobs $ uploaded r)
                , ("downloaded", Just $ itobs $ downloaded r)
                , ("left",       Just $ itobs $ left r)
                , ("compact",    Just "1")
                ]
    where itobs = C.pack . show

makeTrackerRequest :: TrackerRequest -> IO (Either LB.ByteString TrackerResponse)
makeTrackerRequest r = do
    let options = makeOptions r
    request' <- parseRequest $ announce r
    let request = setRequestMethod "GET"
                $ setRequestQueryString options
                    request'
    responseBody <- getResponseBody <$> httpLBS request
    print responseBody
    return $ parseTrackerResponse responseBody

parseTrackerResponse :: LB.ByteString -> Either LB.ByteString TrackerResponse
parseTrackerResponse bs =
    case failureReason of
        Nothing -> Right $ TrackerResponse wait_interval peerList
        Just s  -> case s of BString s -> Left s
    where
        failureReason = lookupBDict "failure_reason" bdict :: Maybe BEncode
        bdict         = fromJust $ bRead bs :: BEncode
        wait_interval = case fromJust $ lookupBDict "interval" bdict of BInt x -> x
        peerList      = fromJust $ parseCompactPeerList 
                        <$> lookupBDict "peers" bdict


-- The first 4 bytes contain the 32-bit ipv4 address. 
-- The remaining two bytes contain the port number. 
-- Both address and port use network-byte order.
parseCompactPeerList :: BEncode -> [Peer]
parseCompactPeerList (BString "") = []
parseCompactPeerList (BString s)  = 
    getPeer (LB.take 6 s) : parseCompactPeerList (BString $ LB.drop 6 s)
    where
        getPeer :: LB.ByteString -> Peer
        getPeer bs = mkPeer ip port
                where (ipBS, portBS) = LB.splitAt 4 bs
                      ip      = fromHostAddress $ runGet getIp ipBS
                      port    = fromIntegral $ runGet getPort portBS
                      getIp   = getWord32le
                      getPort = getWord16le

newTrackerRequest :: String -> B.ByteString -> B.ByteString 
                  -> Int -> TrackerRequest
newTrackerRequest announce infoHash peerId listenPort =
    TrackerRequest 
        { announce   = announce
        , infoHash   = infoHash
        , peerId     = peerId
        , listenPort = listenPort
        , uploaded   = 0
        , downloaded = 0
        , left       = 0
        }