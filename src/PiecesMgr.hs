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
import           Debug.Trace
import           Path
import           System.IO

import           Data.Function                ((&))

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
            { fromPeer :: TChan PeerToPiecesMgr
            , toPeer   :: TChan PiecesMgrToPeer
            } deriving (Eq)

run :: PiecesMgrM a -> PiecesMgrEnv -> IO a
run m env = runStdoutLoggingT $  runReaderT m env

listenerLoop :: PiecesMgrM ()
listenerLoop = asks peers >>= liftIO . readTVarIO >>= mapM_ handlePeer  >> listenerLoop

handlePeer :: Peer -> PiecesMgrM ()
handlePeer peer = do
    "Handling peer " & logPiecesMgr
    inMsg <- liftIO $ atomically $ readTChan $ fromPeer peer
    handleMessage peer inMsg

handleMessage :: Peer -> PeerToPiecesMgr -> PiecesMgrM ()
handleMessage peer inMsg = do
    "Handling " ++ show inMsg & logPiecesMgr
    env <- ask
    case inMsg of
        DonePiece ix bs -> writePiece ix bs

writePiece :: Int -> BS.ByteString-> PiecesMgrM ()
writePiece ix bs = undefined

getPieceOffset :: Int -> PiecesMgrM Integer
getPieceOffset ix = (* fromIntegral ix) <$> asks (fromIntegral . tPieceLength . tInfo . torrent)

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

logPiecesMgr s = traceM $ "PiecesMgr :: " ++ s
