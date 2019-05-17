module PiecesManager where

import qualified Data.ByteString.Lazy as LBS
import           Data.List            (unfoldr)
import           Data.Torrent         (TorrentInfo, tPieces)

data Piece = Piece
             { hash :: LBS.ByteString
             , have :: Bool
             } deriving (Show)

data PiecesManager = PiecesManager
                    { pieceCount :: Int
                    , pieces     :: [Piece]
                    } deriving (Show)

mkPiecesManagerFromInfo :: TorrentInfo -> PiecesManager
mkPiecesManagerFromInfo info = PiecesManager n ps
    where n  = fromIntegral $ LBS.length (tPieces info) `div` 20
          ps = mkPieces $ tPieces info

          mkPieces :: LBS.ByteString -> [Piece]
          mkPieces = unfoldr f
            where f :: LBS.ByteString -> Maybe (Piece, LBS.ByteString)
                  f bs
                    | bs == LBS.empty = Nothing
                    | otherwise       = let (h, bs') = LBS.splitAt 20 bs
                                        in Just (Piece h False, bs')
