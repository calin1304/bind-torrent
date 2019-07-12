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
import qualified Data.Torrent                 as Torrent

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.STM
import           Path
import           System.IO

import           InternalMessage

import           Types                        (PieceIx, PieceOffset,
                                               PieceRequestLen)

type PiecesMgrM a = ReaderT PiecesMgrEnv (LoggingT IO) a

data PiecesMgrEnv = PiecesMgrEnv
                       { root       :: Path Abs Dir
                       , pieceCount :: Int
                       , peers      :: TVar [Peer]
                       , pieces     :: [BS.ByteString]
                       , pieceLen   :: Int
                       , tinfo      :: Torrent.TorrentInfo
                       }

data Peer = Peer
            { fromPeer   :: TChan PeerToPiecesMgr
            , toPeer     :: TChan PiecesMgrToPeer
            , peerPieces :: [Bool]
            } deriving (Eq)

run :: PiecesMgrM a -> PiecesMgrEnv -> IO a
run m env = runStdoutLoggingT $  runReaderT m env

listenerLoop :: (MonadReader PiecesMgrEnv m, MonadIO m, MonadLogger m) => m ()
listenerLoop = ask >>= liftIO . readTVarIO . peers >>= mapM_ handlePeer  >> listenerLoop

handlePeer :: (MonadReader PiecesMgrEnv m, MonadIO m, MonadLogger m) => Peer -> m ()
handlePeer peer = do
    inMsg <- liftIO $ atomically $ readTChan $ fromPeer peer
    handleMessage peer inMsg

handleMessage :: (MonadReader PiecesMgrEnv m, MonadIO m, MonadLogger m) => Peer -> PeerToPiecesMgr -> m ()
handleMessage peer inMsg = do
    logDebugN $ mconcat ["Handling message ", Text.pack $ show inMsg]
    conf <- ask
    let len = fromIntegral $ pieceLen conf
    case inMsg of
        RequestNextPiece -> do
            let ix = nextPieceToDownload peer
            liftIO $ atomically $ writeTChan (toPeer peer) (NextPiece ix len)
        DonePiece ix bs -> do
            let peer' = updateHavePiece peer ix
            liftIO $ atomically $ do
                ps <- readTVar (peers conf)
                let ps' = updatePeerInPeerList ps peer peer'
                writeTVar (peers conf) ps'
            writePiece ix bs

updatePeerInPeerList :: [Peer] -> Peer -> Peer -> [Peer]
updatePeerInPeerList ps p p' = let (Just i) = p `List.elemIndex` ps
                                in take (i-1) ps ++ [p'] ++ drop (i+1) ps

nextPieceToDownload :: Peer -> PieceIx
nextPieceToDownload peer = fst $ head $ dropWhile snd $ zip [1..] (peerPieces peer)

updateHavePiece :: Peer -> PieceIx -> Peer
updateHavePiece peer ix = let ps = peerPieces peer
                              i = fromIntegral ix
                              ps' = take (i-1) ps ++ [True] ++ drop (i+1) ps
                           in peer {peerPieces = ps'}

writePiece :: (MonadReader PiecesMgrEnv m, MonadIO m, MonadLogger m) => PieceIx -> BS.ByteString -> m ()
writePiece ix bs = do
    logDebugN $ mconcat ["Writing piece ", Text.pack (show ix)]
    conf <- ask
    let h = pieces conf !! fromIntegral ix
    unless (isValidPiece h bs) $ error "Invalid piece"
    fname <- getPieceFileName
    off <- getPieceOffset ix
    handle <- liftIO $ openFile fname ReadMode
    liftIO $ hSeek handle AbsoluteSeek off
    liftIO $ BS.hPut handle bs
    liftIO $ hClose handle

-- FIXME
getPieceFileName :: (MonadReader PiecesMgrEnv m) => m FilePath
getPieceFileName = C.unpack . Torrent.tName <$> asks tinfo

getPieceOffset :: (MonadReader PiecesMgrEnv m) => PieceIx -> m Integer
getPieceOffset ix = (*) (fromIntegral ix) . Torrent.tLength <$> asks tinfo

-- TODO: Change first argument type to Digest SHA1
isValidPiece :: BS.ByteString -> BS.ByteString -> Bool
isValidPiece = (==) . SHA1.hash

newEnvFromInfo :: (MonadIO m)
               => TorrentInfo -> Path Abs Dir -> Int -> m PiecesMgrEnv
newEnvFromInfo tinfo root pieceLen =
    case tinfo of
        Torrent.MultiFile{} -> error "Multifile torrent not supported"
        Torrent.SingleFile{} -> liftIO $ PiecesMgrEnv root n <$> newTVarIO []
                                        <*> pure hs <*> pure pieceLen <*> pure tinfo
    where n  = fromIntegral $ LBS.length (Torrent.tPieces tinfo) `div` 20
          hs = List.unfoldr f $ LBS.toStrict $ Torrent.tPieces tinfo
          f :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
          f bs
              | bs == BS.empty = Nothing
              | otherwise       = Just $ BS.splitAt 20 bs

start :: PiecesMgrEnv -> IO ()
start env = void $ async $ run start' env

start' :: (MonadReader PiecesMgrEnv m, MonadIO m) => m ()
start' = do
    conf <- ask
    loop <- liftIO $ async $ run listenerLoop conf
    liftIO $ waitAnyCancel [loop]
    return ()
