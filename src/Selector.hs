{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Selector where

import qualified Data.Text                    as Text

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.Logger
import           Control.Monad.Random.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import           Debug.Trace
import           System.Random

import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)

import           InternalMessage
import qualified Peer                         as Peer

import           Peer                         (Peer)

data SelectorEnv = SelectorEnv
                   {  maxActiveConns :: Int
                   , peers           :: [(Peer, TChan SelectorToPeer)]
                   , activeConns     :: TVar Int
                   }

type SelectorM a = ReaderT SelectorEnv (LoggingT IO) a

newEnv :: [(Peer, TChan SelectorToPeer)] -> Int -> IO SelectorEnv
newEnv ps maxConns = SelectorEnv maxConns ps <$> newTVarIO 0

selectorLoop :: SelectorM ()
selectorLoop = do
    env <- ask
    let potentialPeers = undefined -- peersHavingPieces `Set.intersect` peersChoking
        peersHavingPieces = []

        peersChoking = filter isChoking (peers env)
        isChoking (p, chan) = fromJust $ Peer._isChoking <$> Peer._maybeActive p

        notInterestedPeers = filter notInterested (peers env)
        notInterested (p, chan) = fromJust $ Peer._isInterested <$> Peer._maybeActive p

    logSelector $ mconcat ["Current potential peers: ", Text.pack $ show (map fst (peers env))]
    selectionSize <- liftIO $ (maxActiveConns env -) <$> readTVarIO (activeConns env)

    when (selectionSize == 0) $ do
        logSelector "Dropping random peer"
        dropRandomPeer

    logSelector "Selecting random peer"
    selectRandomPeer

    liftIO $ threadDelay timeout
    selectorLoop
    where timeout = 1000000 * 10

dropRandomPeer :: (MonadReader SelectorEnv m, MonadIO m) => m ()
dropRandomPeer = do
    env <- ask
    withRandomPeer $ \(p, chan) -> liftIO $ atomically $ do
                                      writeTChan chan (SetInterest False)
                                      modifyTVar (activeConns env) (+ (-1))

selectRandomPeer :: (MonadReader SelectorEnv m, MonadIO m) => m ()
selectRandomPeer = do
    env <- ask
    withRandomPeer $ \(p, chan) -> liftIO $ atomically $ do
                                        writeTChan chan (SetInterest True)
                                        modifyTVar (activeConns env) (+ 1)

withRandomPeer :: (MonadReader SelectorEnv m, MonadIO m) => ((Peer, TChan SelectorToPeer)-> m c) -> m c
withRandomPeer f = asks peers >>= \ps -> liftIO (randomRIO (0, length ps)) >>= f . (ps !!)

run :: SelectorM a -> SelectorEnv -> IO a
run m env = runStdoutLoggingT $ runReaderT m env

start :: SelectorEnv -> IO ()
start = run start'

start' :: (MonadReader SelectorEnv m, MonadIO m, MonadLogger m) => m ()
start' = do
    env <- ask
    loop <- liftIO $ async $ run selectorLoop env
    liftIO $ wait loop
    traceM "Should not be here in Selector start'"

logSource :: Text
logSource = "Selector"

logSelector :: (MonadLogger m) => Text -> m ()
logSelector = logDebugNS logSource