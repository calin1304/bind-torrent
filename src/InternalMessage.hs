module InternalMessage where

import           Data.ByteString

import           Types           (PieceIx, PieceOffset, PieceRequestLen)

data PeerToPiecesMgr = RequestNextPiece
                     | DonePiece PieceIx ByteString
                     | DoneBlock PieceIx PieceOffset ByteString
                       deriving (Show)

data PiecesMgrToPeer = NextPiece PieceIx PieceRequestLen
                       deriving (Show)

newtype SelectorToPeer = SetInterest Bool
                         deriving (Show)
