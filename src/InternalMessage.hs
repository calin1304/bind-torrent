module InternalMessage
       ( PiecesMgrMessage(..)
       , SessionMessage(..)
       ) where

import           Data.ByteString

data PiecesMgrMessage = HavePiece Int ByteString

data SessionMessage = Cancel
