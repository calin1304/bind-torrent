module InternalMessage where

import           Data.ByteString

import           Internal.Message

data PeerToPiecesMgr = RequestNextPiece
                     | DonePiece PieceIx ByteString
                     | DoneBlock PieceIx PieceOffset ByteString
                       deriving (Show)

data PiecesMgrToPeer = NextPiece PieceIx PieceRequestLen
                       deriving (Show)

newtype SelectorToPeer = SetInterest Bool
                         deriving (Show)
