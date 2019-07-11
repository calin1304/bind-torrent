{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Internal.Handshake where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word       (Word8)
import Control.Lens
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.ByteString (Parser)

import           Peer            (PeerId)
import           Types           (InfoHash)

data Handshake = Handshake
    { _pstrlen  :: Word8
    , _pstr     :: BS.ByteString
    , _reserved :: BS.ByteString
    , _infoHash :: InfoHash
    , _peerId   :: PeerId
    } deriving(Show, Eq)
makeLenses ''Handshake

instance Binary Handshake where
    put (Handshake pstrlen pstr reserved infoHash peerId) = do
        putWord8 pstrlen
        putByteString pstr
        putByteString reserved
        putByteString infoHash
        putByteString peerId

    get = Handshake <$> getWord8 
                    <*> getByteString 19 
                    <*> getByteString 8
                    <*> getByteString 20
                    <*> getByteString 20

new :: InfoHash -> PeerId -> Handshake
new = Handshake 19 "BitTorrent protocol" (BS.replicate 8 0)

parser :: Parser Handshake
parser = Handshake <$> AP.word8 19 
                   <*> AP.string "BitTorrent protocol" 
                   <*> AP.take 8 
                   <*> AP.take 20 
                   <*> AP.take 20    
