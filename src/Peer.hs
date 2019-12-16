module Peer
       ( start
       , newConfig
       )
       where

import           Control.Concurrent.STM.TBChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Data.Torrent

import           Conduit                      (mapC)
import           Control.Concurrent.Async     (async, waitAnyCatchCancel)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Binary                  (encode)
import           Data.Conduit                 ((.|))
import           Data.Conduit.Attoparsec      (conduitParser)
import           Data.Conduit.Network         (sinkSocket, sourceSocket)
import           Network.Socket               (Socket)

import           Internal.Peer

import           InternalMessage              (PiecesMgrMessage (..))
import           MovingWindow                 (MovingWindow)
import           Types                        (InfoHash, PeerId)

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set

import qualified Message

newConfig 
    :: InfoHash 
    -> TorrentInfo 
    -> PeerId 
    -> Socket 
    -> TVar PieceSet
    -> TVar MovingWindow 
    -> TBChan PiecesMgrMessage
    -> Int
    -> IO PeerEnv
newConfig ih tinfo pid sock ourPs mw toPiecesMgr blockSize =
    PeerEnv ih tinfo pid siC soC blockSize ourPs mw toPiecesMgr
        <$> newTVarIO False
        <*> newTVarIO (PeerState True False)
        <*> newPiecesInfo
        <*> newBlocksInfo
        <*> newTVarIO True
    where siC = sourceSocket sock .| conduitParser Message.parser .| mapC snd
          soC = mapC (LBS.toStrict . encode) .| sinkSocket sock
          newPiecesInfo = PiecesInfo <$> newTVarIO Nothing <*> newTVarIO Set.empty
          newBlocksInfo = BlocksInfo <$> newTVarIO Set.empty <*> newTVarIO Map.empty <*> newTVarIO Set.empty

start :: PeerEnv -> IO ()
start = runReaderT $ do
    sendMessage =<< Message.Handshake <$> asks peInfoHash <*> asks pePeerId
    env <- ask
    void $ liftIO $ (mapM (async . flip runReaderT env) >=> waitAnyCatchCancel) [mainLoop]

