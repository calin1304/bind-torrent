module InternalMessage
       ( PeerToPiecesMgr(..)
       , PiecesMgrToPeer
       ) where

import           Data.ByteString

data PeerToPiecesMgr = RequestNextPiece
                     | DonePiece Int ByteString
                     | DoneBlock Int Int ByteString
                     deriving (Show)

data PiecesMgrToPeer = NextPiece Int Int
                     deriving (Show)
