module PiecesManager where

-- import Data.Torrent(TorrentInfo, tPieces)
-- import qualified Data.ByteString.Lazy as L

-- data Piece = Piece 
--              { hash :: L.ByteString
--              , have :: Bool
--              } deriving (Show)

-- data PiecesManager = PiecesManager 
--                     { pieceCount :: Int
--                     , pieces :: [Piece]
--                     } deriving (Show)

-- mkPiecesManagerFromInfo :: TorrentInfo -> PiecesManager
-- mkPiecesManagerFromInfo info = 
--     let n  = fromIntegral $ L.length (tPieces info) `div` 20
--         ps = mkPieces $ tPieces info
        
--         mkPieces :: L.ByteString -> [Piece]
--         mkPieces bs 
--             | L.length bs == 0 = [] 
--             | otherwise        = let (h, bs') = L.splitAt 20 bs in (Piece h False) : mkPieces bs'
--     in PiecesManager n ps