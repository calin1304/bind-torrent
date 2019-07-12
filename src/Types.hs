module Types where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Data.Word             (Word32)

type InfoHash = BS.ByteString
type Announce = BS.ByteString
type PeerId = C.ByteString

type PieceIx         = Word32
type PieceOffset     = Word32
type PieceRequestLen = Word32
