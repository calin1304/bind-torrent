{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tracker where

import           Control.Lens
import           Data.BEncode          (BEncode (..), bRead)
import           Data.Binary.Get
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as LBS
import           Data.IP               (fromHostAddress)
import           Network.HTTP.Simple
import           Numeric               (showInt)

import           BEncoding
import           Peer                  (Peer, PeerId, mkPeer)
import           Types                 (Announce, InfoHash)

data TrackerRequest = TrackerRequest
    { _announce   :: Announce
    , _infoHash   :: InfoHash
    , _peerId     :: PeerId
    , _listenPort :: Int
    , _uploaded   :: Int
    , _downloaded :: Int
    , _left       :: Int
    } deriving(Show)
makeLenses ''TrackerRequest

data TrackerResponse = TrackerResponse
    { _interval :: Integer
    , _peers    :: [Peer]
    } deriving(Show)
makeLenses ''TrackerResponse

mkTrackerRequest :: Announce -> InfoHash -> PeerId -> Int -> TrackerRequest
mkTrackerRequest an ih pi port = TrackerRequest 
                               { _announce = an
                               , _infoHash = ih
                               , _peerId = pi
                               , _listenPort = port
                               , _uploaded = 0
                               , _downloaded = 0
                               , _left = 0
                               }

mkRequestOptions :: TrackerRequest -> [(BS.ByteString, Maybe BS.ByteString)]
mkRequestOptions r = [ ("info_hash",  Just $         r ^. infoHash)
                     , ("peer_id",    Just $         r ^. peerId )
                     , ("port",       Just $ itobs $ r ^. listenPort)
                     , ("uploaded",   Just $ itobs $ r ^. uploaded)
                     , ("downloaded", Just $ itobs $ r ^. downloaded)
                     , ("left",       Just $ itobs $ r ^. left)
                     , ("compact",    Just "1")
                     ]
    where itobs = C.pack . show

sendRequest :: TrackerRequest -> IO (Either LBS.ByteString TrackerResponse)
sendRequest r = do
    let options = mkRequestOptions r
    request' <- parseRequest $ C.unpack $ r ^. announce
    let request = setRequestMethod "GET"
                $ setRequestQueryString options
                    request'
    responseBody <- getResponseBody <$> httpLBS request
    return $ parseTrackerResponse responseBody

parseTrackerResponse :: LBS.ByteString -> Either LBS.ByteString TrackerResponse
parseTrackerResponse bs =
    case failureReason of
        Nothing          -> Right $ TrackerResponse wait_interval peerList
        Just (BString s) -> Left s
    where
        failureReason = lookupBDict "failure_reason" bdict :: Maybe BEncode
        wait_interval = let Just (BInt x) = lookupBDict "interval" bdict in x
        peerList      = let Just x = parseCompactPeerList <$> lookupBDict "peers" bdict
                        in x
        bdict         = let Just x = bRead bs in x

-- The first 4 bytes contain the 32-bit ipv4 address.
-- The remaining two bytes contain the port number.
-- Both address and port use network-byte order.
parseCompactPeerList :: BEncode -> [Peer]
parseCompactPeerList (BString "") = []
parseCompactPeerList (BString s)  =
    getPeer (LBS.take 6 s) : parseCompactPeerList (BString $ LBS.drop 6 s)
    where
        getPeer :: LBS.ByteString -> Peer
        getPeer bs = mkPeer ip port
                where (ipBS, portBS) = LBS.splitAt 4 bs
                      ip      = fromHostAddress $ runGet getIp ipBS
                      port    = fromIntegral $ runGet getPort portBS
                      getIp   = getWord32le
                      getPort = getWord16le
