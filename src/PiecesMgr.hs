{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module PiecesMgr where

import           Data.Torrent                 (TorrentInfo)

import qualified Crypto.Hash.SHA1             as SHA1
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as C
import qualified Data.List                    as List
import qualified Data.Text                    as Text

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Torrent
import           Path
import           System.IO

import           InternalMessage

import           Types                        (PieceIx, PieceOffset,
                                               PieceRequestLen)

data PieceSize = Normal | Specific Int

data Piece = Piece
    { pieceIx   :: !Int
    , pieceSize :: !PieceSize
    , pieceBS   :: !BS.ByteString
    }

type PiecesMgrM a = ReaderT PiecesMgrEnv (LoggingT IO) a

data PiecesMgrEnv = PiecesMgrEnv
    { root       :: !(Path Abs Dir)
    , pieceCount :: !Int
    , pieceLen   :: !Int
    , torrent    :: !Torrent
    , peers      :: !(TVar [Peer])
    , pieces     :: !(TVar [Piece])
    }

data Peer = Peer
            { fromPeer   :: TChan PeerToPiecesMgr
            , toPeer     :: TChan PiecesMgrToPeer
            , peerPieces :: [Bool]
            } deriving (Eq)

run :: PiecesMgrM a -> PiecesMgrEnv -> IO a
run m env = runStdoutLoggingT $  runReaderT m env

listenerLoop :: PiecesMgrM ()
listenerLoop = ask >>= liftIO . readTVarIO . peers >>= mapM_ handlePeer  >> listenerLoop

handlePeer :: Peer -> PiecesMgrM ()
handlePeer peer = do
    inMsg <- liftIO $ atomically $ readTChan $ fromPeer peer
    handleMessage peer inMsg

handleMessage :: Peer -> PeerToPiecesMgr -> PiecesMgrM ()
handleMessage peer inMsg = do
    env <- ask
    let len = fromIntegral $ pieceLen env
    case inMsg of
        RequestNextPiece -> do
            let ix = nextPieceToDownload peer
            liftIO $ atomically $ writeTChan (toPeer peer) (NextPiece ix len)
        DonePiece ix bs -> addPiece ix >> liftIO (readTVarIO $ pieces env) >>= writePiece . (!! ix)

addPiece :: Int -> PiecesMgrM ()
addPiece = undefined

nextPieceToDownload :: Peer -> Int
nextPieceToDownload peer = fst $ head $ dropWhile snd $ zip [1..] (peerPieces peer)

writePiece :: (MonadReader PiecesMgrEnv m, MonadIO m, MonadLogger m) => Piece -> m ()
writePiece piece = do
    env <- ask
    off <- getPieceOffset piece
    handle <- liftIO $ openFile (C.unpack . tName . tInfo . torrent $ env) ReadMode
    liftIO $ hSeek handle AbsoluteSeek off
    liftIO $ BS.hPut handle (pieceBS piece)
    liftIO $ hClose handle

getPieceOffset :: (MonadReader PiecesMgrEnv m) => Piece -> m Integer
getPieceOffset p = fromIntegral . (* pieceIx p) <$> asks (fromIntegral . tLength . tInfo . torrent)

-- TODO: Change first argument type to Digest SHA1
isValidPiece :: BS.ByteString -> BS.ByteString -> Bool
isValidPiece = (==) . SHA1.hash

newEnvFromInfo :: Torrent -> Path Abs Dir -> Int -> IO PiecesMgrEnv
newEnvFromInfo t root pieceLen =
    case tInfo t of
        MultiFile{}  -> error "Multifile torrent not supported"
        SingleFile{} -> liftIO $ PiecesMgrEnv root n pieceLen t <$> newTVarIO [] <*> newTVarIO []
    where n  = fromIntegral $ LBS.length (tPieces $ tInfo t) `div` 20
          f :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
          f bs
              | bs == BS.empty = Nothing
              | otherwise       = Just $ BS.splitAt 20 bs

start :: PiecesMgrEnv -> IO ()
start = run listenerLoop

-- | Get length of piece given the piece index,
-- default piece length and total length of file.
pieceLength :: Int -> Int -> Integer -> Int
pieceLength ix plen total =
    if isLastPiece
        then fromIntegral total `mod` plen
        else plen
    where isLastPiece = fromIntegral total `mod` plen > 0 && ix == fromIntegral total `div` plen

-- | Get length of a block given the index
-- from begining of piece, default block length
-- and total piece size.
blockLength :: Int -> Int -> Integer -> Int
blockLength = pieceLength
