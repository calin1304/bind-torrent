module Internal.Message where

import           Network.Socket        (PortNumber)
import Data.Word(Word32)
import qualified Data.ByteString as BS

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
            deriving (Show)

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