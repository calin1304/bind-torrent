module Message where

import           Data.Binary                (Binary, Get, Put, get, put)
import           Data.Binary.Get            (getByteString, getWord16be,
                                             getWord32be, getWord8)
import           Data.Binary.Put            (putByteString, putWord16be,
                                             putWord32be, putWord8)
import qualified Data.ByteString            as BS
import           Data.Word                  (Word16, Word32, Word8)
import           Network.Socket             (PortNumber)

import qualified Data.Attoparsec.Binary     as AP
import qualified Data.Attoparsec.ByteString as AP

type PieceIx         = Word32
type PieceOffset     = Word32
type PieceRequestLen = Word32

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceIx
             | BitField BS.ByteString
             | Request PieceIx PieceOffset PieceRequestLen
             | Piece PieceIx PieceOffset BS.ByteString
             | Cancel PieceIx PieceOffset PieceRequestLen
             | Port PortNumber
             deriving (Eq, Show)

instance Binary Message where
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
                    5 -> do
                        bf <- get :: Get BS.ByteString
                        return $ BitField bf
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
    put (BitField bf)             = do
        putWord32be (fromIntegral (1 + BS.length bf) :: Word32)
        putWord8 5
        put bf
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

parser :: AP.Parser Message
parser = do
    len <- fromIntegral <$> AP.anyWord32be
    msgId <- AP.anyWord8
    case msgId of
        0 -> return KeepAlive
        1 -> return Unchoke
        2 -> return Interested
        3 -> return NotInterested
        4 -> Have <$> AP.anyWord32be
        5 -> BitField <$> AP.take (len - 1)
        6 -> Request <$> AP.anyWord32be <*> AP.anyWord32be <*> AP.anyWord32be
        7 -> Piece <$> AP.anyWord32be <*> AP.anyWord32be <*> AP.take (len - 9)
        8 -> Cancel <$> AP.anyWord32be <*> AP.anyWord32be <*> AP.anyWord32be
        9 -> Port . fromIntegral <$> AP.anyWord16be
