module Main (main) where

import           Web.Scotty

import           System.Environment   (getArgs)

import qualified Data.ByteString.Lazy as LBS

import qualified Session


--server = scotty 3000 $ do
--    get "/" $
--        file "ui/index.html"
--    get "/css/main.css" $
--        file "ui/css/main.css"
--    get "/js/main.js" $
--        file "ui/js/main.js"
--    post "/loadTorrent" $ do
--        meta <- body :: ActionM LBS.ByteString
--        liftIO $ print meta
--        config <- liftIO $ Session.newEnvFromMeta meta
--        let ih = decodeUtf8 $ LBS.fromStrict $ Session.infoHash config
--        let resp = M.singleton "info hash" ih :: M.Map Text Text
--        json resp

main :: IO ()
main = do
    args <- getArgs
    let fname = head args
    LBS.readFile fname >>= Session.newEnvFromMeta >>= Session.start
