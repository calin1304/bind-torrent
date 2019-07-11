module Types where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C

type InfoHash = BS.ByteString
type Announce = BS.ByteString
type PeerId = C.ByteString
