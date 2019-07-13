{-# LANGUAGE OverloadedStrings #-}

module Message where

import qualified Data.Attoparsec.Binary     as AP
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Bits                  as Bits
import qualified Data.ByteString            as BS
import qualified Data.Set                   as Set

import           Data.Binary                (Binary, Get, Put, get, put)
import           Data.Binary.Get            (getByteString, getWord16be,
                                             getWord32be, getWord8)
import           Data.Binary.Put            (putByteString, putWord16be,
                                             putWord32be, putWord8)
import           Data.Set                   (Set)
import           Data.Word                  (Word16, Word32, Word8)
import           Network.Socket             (PortNumber)

import           Types                      (InfoHash, PeerId, PieceIx,
                                             PieceOffset, PieceRequestLen)

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceIx
             | BitField !(Set Int)
             | Request PieceIx PieceOffset PieceRequestLen
             | Piece PieceIx PieceOffset BS.ByteString
             | Cancel PieceIx PieceOffset PieceRequestLen
             | Port PortNumber
             | Handshake InfoHash PeerId
             deriving (Eq, Show)

instance Binary Message where
    -- TODO: Handle Handshake message type
    get = do
        length <- getWord32be
        if length == 0
            then return KeepAlive
            else do
                id <- getWord8
                case id of
                    0 -> return Choke
                    1 -> return Unchoke
                    2 -> return Interested
                    3 -> return NotInterested
                    4 -> Have <$> getWord32be
                    5 -> BitField .  bitfieldToSet <$> getByteString (fromIntegral length - 1)
                    6 -> Request <$> getWord32be <*> getWord32be <*> getWord32be
                    7 -> do
                        ix    <- getWord32be
                        begin <- getWord32be
                        block <- get :: Get BS.ByteString
                        return $ Piece ix begin block
                    8 -> Cancel <$> getWord32be <*> getWord32be <*> getWord32be
                    9 -> do
                        portNumber <- getWord16be :: Get Word16
                        return $ Port (fromIntegral portNumber)

    put KeepAlive                 = putWord32be 0
    put Choke                     = putWord32be 1 >> putWord8 0
    put Unchoke                   = putWord32be 1 >> putWord8 1
    put Interested                = putWord32be 1 >> putWord8 2
    put NotInterested             = putWord32be 1 >> putWord8 3
    put (Have ix)                 = do
        putWord32be 5
        putWord8 4
        putWord32be ix
    put (BitField bf)             = undefined
    put (Request ix begin length) = do
        putWord32be 13
        putWord8 6
        putWord32be ix
        putWord32be begin
        putWord32be length
    put (Piece ix begin block)    = do
        putWord32be (fromIntegral (9 + BS.length block) :: Word32)
        putWord8 (7 :: Word8)
        putWord32be ix
        putWord32be begin
        put block
    put (Cancel ix begin length)  = do
        putWord32be 13
        putWord8 8
        putWord32be ix
        putWord32be begin
        putWord32be length
    put (Port portNumber)         = do
        putWord32be 3
        putWord8 9
        put (fromIntegral portNumber :: Word16)

    put (Handshake infoHash peerId) = do
        putWord8 19
        putByteString "BitTorrent protocol"
        putByteString (BS.replicate 8 0)
        putByteString infoHash
        putByteString peerId

parser :: AP.Parser Message
parser = AP.choice [handshakeParser, messageParser]

handshakeParser :: AP.Parser Message
handshakeParser = do
    _ <- AP.word8 19 >> AP.string "BitTorrent protocol" >> AP.take 8
    Handshake <$> AP.take 20 <*> AP.take 20

messageParser :: AP.Parser Message
messageParser = do
    len <- fromIntegral <$> AP.anyWord32be
    msgId <- AP.anyWord8
    case msgId of
        0 -> return KeepAlive
        1 -> return Unchoke
        2 -> return Interested
        3 -> return NotInterested
        4 -> Have <$> AP.anyWord32be
        5 -> BitField . bitfieldToSet <$> AP.take (len - 1)
        6 -> Request <$> AP.anyWord32be <*> AP.anyWord32be <*> AP.anyWord32be
        7 -> Piece <$> AP.anyWord32be <*> AP.anyWord32be <*> AP.take (len - 9)
        8 -> Cancel <$> AP.anyWord32be <*> AP.anyWord32be <*> AP.anyWord32be
        9 -> Port . fromIntegral <$> AP.anyWord16be

bitfieldToSet :: BS.ByteString -> Set Int
bitfieldToSet bs = Set.fromList $ concat groups
    where tests = map (\w -> filter (Bits.testBit w . (7-)) [0..7]) (BS.unpack bs)
          groups = zipWith (map . flip (+)) [0, 8 ..] tests
