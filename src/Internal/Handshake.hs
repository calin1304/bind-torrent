{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Internal.Handshake where

import           Data.Binary     (Binary, get, getWord8, put, putWord8)
import qualified Data.ByteString as BS
import           Data.Word       (Word8)
import Control.Lens

import           Peer            (PeerId)
import           Types           (InfoHash)

data Handshake = Handshake
    { _pstrlen  :: Word8
    , _pstr     :: BS.ByteString
    , _reserved :: BS.ByteString
    , _infoHash :: InfoHash
    , _peerId   :: PeerId
    } deriving(Show)
makeLenses ''Handshake

instance Binary Handshake where
    put (Handshake pstrlen pstr reserved infoHash peerId) = do
        putWord8 pstrlen
        put pstr
        put reserved
        put infoHash
        put peerId

    get = do
        pstrlen <- getWord8
        pstr <- get
        reserved <- get
        infoHash <- get
        Handshake pstrlen pstr reserved infoHash <$> get

mkHandshake :: InfoHash -> PeerId -> Handshake
mkHandshake = Handshake pstrlen pstr reserved
    where
        pstrlen  = 19 :: Word8
        pstr     = "BitTorrent protocol"
        reserved = BS.replicate 8 0

isValidHandshake :: Handshake -> InfoHash -> Bool
isValidHandshake h expectedInfoHash = isValidPstr && isValidInfoHash
    where isValidPstr     = h ^. pstrlen == 19 && 
                            h ^. pstr == "BitTorrent protocol" 
          isValidInfoHash = h ^. infoHash == expectedInfoHash