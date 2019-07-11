{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS

import           Data.Map.Strict         as M
import           Data.Monoid             (mconcat)
import           Debug.Trace
import           System.Environment      (getArgs)

-- Text
import           Data.Text.Lazy          (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8)

-- Web server
import           Web.Scotty

-- Lib
import qualified Session                 as Session

server = scotty 3000 $ do
    get "/" $
        file "ui/index.html"
    get "/css/main.css" $
        file "ui/css/main.css"
    get "/js/main.js" $
        file "ui/js/main.js"
    post "/loadTorrent" $ do
        meta <- body :: ActionM LBS.ByteString
        liftIO $ print meta
        config <- liftIO $ Session.newEnvFromMeta meta
        let ih = decodeUtf8 $ LBS.fromStrict $ Session.infoHash config
        let resp = M.singleton "info hash" ih :: M.Map Text Text
        json resp

main :: IO ()
main = do
    [fname] <- getArgs
    LBS.readFile fname >>= Session.newEnvFromMeta >>= Session.start
    traceM "Should not get here"
