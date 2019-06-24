module PiecesManager where

import           Data.Torrent         (TorrentInfo)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as List
import qualified Data.Torrent         as Torrent

data Piece = Piece
             { hash :: LBS.ByteString
             , have :: Bool
             , freq :: Int
             } deriving (Show)

data PiecesManager = PiecesManager
                    { pieceCount :: Int
                    , pieces     :: [Piece]
                    } deriving (Show)

newFromInfo :: TorrentInfo -> PiecesManager
newFromInfo info = PiecesManager n ps
    where n  = fromIntegral $ LBS.length (Torrent.tPieces info) `div` 20
          ps = mkPieces $ Torrent.tPieces info

          mkPieces :: LBS.ByteString -> [Piece]
          mkPieces = List.unfoldr f
            where f :: LBS.ByteString -> Maybe (Piece, LBS.ByteString)
                  f bs
                    | bs == LBS.empty = Nothing
                    | otherwise       = let (h, bs') = LBS.splitAt 20 bs
                                         in Just (Piece h False 0, bs')

updateFromBitField :: PiecesManager -> [Bool] -> PiecesManager
updateFromBitField pm bs = pm'
    where pm' = pm { pieces = map update $ zip (pieces pm) bs}
          update (p, b) = if b then p {freq = (freq p) + 1} else p

nextMissingPiece :: PiecesManager -> Piece
nextMissingPiece pm = head $ List.sortOn freq pieces'
    where pieces' = filter (not . have) (pieces pm)
