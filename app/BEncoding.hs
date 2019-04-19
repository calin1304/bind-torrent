module BEncoding where

import qualified Data.Map as Map

import Data.BEncode(BEncode(..))

lookupBDict :: String -> BEncode -> Maybe BEncode
lookupBDict k (BDict m) = Map.lookup k m