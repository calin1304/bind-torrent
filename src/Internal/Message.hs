module Internal.Message where

import           Data.Binary     (Binary, Get, Put, get, put)
import           Data.Binary.Get (getByteString, getWord16be, getWord32be,
                                  getWord8)
import           Data.Binary.Put (putByteString, putWord16be, putWord32be,
                                  putWord8)
import qualified Data.ByteString as BS
import           Data.Word       (Word16, Word32, Word8)
import           Network.Socket  (PortNumber)

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
                    4 -> do
                        ix <- getWord32be
                        return $ Have ix
                    5 -> do
                        bf <- get :: Get BS.ByteString
                        return $ BitField bf
                    6 -> do
                        ix     <- getWord32be
                        begin  <- getWord32be
                        length <- getWord32be
                        return $ Request ix begin length
                    7 -> do
                        ix    <- getWord32be
                        begin <- getWord32be
                        block <- get :: Get BS.ByteString
                        return $ Piece ix begin block
                    8 -> do
                        ix     <- getWord32be
                        begin  <- getWord32be
                        length <- getWord32be
                        return $ Cancel ix begin length
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

mkMessage :: Message -> BS.ByteString
mkMessage messageType = undefined
--     case messageType of
--         KeepAlive     -> Message 0 Nothing Nothing
--         Choke         -> Message 1 (Just 0) Nothing
--         Unchoke       -> Message 1 (Just 1) Nothing
--         Interested    -> Message 1 (Just 2) Nothing
--         NotInterested -> Message 1 (Just 3) Nothing
--         Have          -> undefined
--         BitField      -> undefined
--         Request       -> undefined
--         Piece         -> undefined
--         Cancel        -> undefined
--         Port          -> undefined
