{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent.STM.TChan
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Torrent
import           Web.Scotty

import           Control.Concurrent.STM.TVar  (TVar, newTVarIO, readTVarIO)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import           System.Environment           (getArgs)

import           InternalMessage              (SessionMessage (..))
import           Session                      (TorrentStatus (..))
import           TorrentInfo

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Map.Strict              as M
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

import qualified Session


data Env = Env
    { envTorrentStatus :: !(TVar (Maybe TorrentStatus))
    , toSession        :: !(TChan SessionMessage)
    }

server :: ReaderT Env IO ()
server = do
    env <- ask
    ts <- asks envTorrentStatus
    liftIO $ scotty 3000 $ do
        get "/" $
            file "ui/index.html"
        get "/css/main.css" $
            file "ui/css/main.css"
        get "/js/main.js" $
            file "ui/js/main.js"
        get "/status" $ do
            maybeStatus <- liftIO $ readTVarIO ts
            json $ fromMaybe (TorrentStatus 0 0) maybeStatus
        post "/loadTorrent" $ do
            -- Start session
            meta <- body :: ActionM LBS.ByteString
            config <- liftIO $ Session.newEnvFromMeta meta ts (toSession env)
            let torrent = Session.sessionTorrent config
            liftIO $ Session.start config
            -- Send torrent info response
            let tname = TE.decodeUtf8 $ LBS.toStrict $ tName $ tInfo torrent
                thash = ""  -- FIXME
                tsize = fromIntegral $ torrentSize torrent
            dateAdded <- liftIO $ T.pack . formatTime defaultTimeLocale "%d-%m-%Y" <$> getCurrentTime
            json $ TorrentInfo tname thash tsize dateAdded
        post "/cancel" $
            liftIO $ atomically $ writeTChan (toSession env) Cancel

newEnv :: IO Env
newEnv = Env <$> newTVarIO Nothing <*> newTChanIO

main :: IO ()
main = do
    args <- getArgs
    case head args of
        "--web" ->
            runReaderT server =<< newEnv
        _ -> do
            Env ts chan <- newEnv
            LBS.readFile (head args) >>= \meta -> Session.newEnvFromMeta meta ts chan >>= Session.start
