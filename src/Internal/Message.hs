module Internal.Message
    ( bitfieldToSet
    ) where

import           Data.Set        (Set)

import qualified Data.Bits       as Bits
import qualified Data.ByteString as BS
import qualified Data.Set        as Set


bitfieldToSet :: BS.ByteString -> Set Int
bitfieldToSet bs = Set.fromList $ concat groups
    where tests = map (\w -> filter (Bits.testBit w . (7-)) [0..7]) (BS.unpack bs)
          groups = zipWith (map . flip (+)) [0, 8 ..] tests

