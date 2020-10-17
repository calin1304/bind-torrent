module Main (main) where

import           Control.Concurrent.STM.TBChan
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Torrent
import qualified Dhall                         (auto, inputFile)
import           Options.Applicative
import           Web.Scotty

import           Control.Concurrent.STM.TVar   (TVar, newTVarIO, readTVarIO)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Maybe                    (fromMaybe)

import           InternalMessage               (SessionMessage (..))
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Session                       (TorrentStatus (..))
import           TorrentInfo

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LC
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

import qualified Session


data Env = Env
    { envTorrentStatus :: TVar (Maybe TorrentStatus)
    , toSession        :: TBChan SessionMessage
    }

data Arguments = Arguments
    { configFile :: FilePath
    , torrentFile  :: FilePath
    }

server :: Arguments -> ReaderT Env IO ()
server args = do
    env <- ask
    ts <- asks envTorrentStatus
    let base = "bind-torrent-web-ui"
    liftIO $ scotty 3000 $ do
        middleware $
            staticPolicy (addBase base)
        get "/" $ file (base ++ "/index.html")
        get "/status" $ do
            maybeStatus <- liftIO $ readTVarIO ts
            json $ fromMaybe (TorrentStatus 0 0) maybeStatus
        post "/loadTorrent" $ do
            -- Start session
            meta <- LC.unpack <$> body :: ActionM String
            -- TODO: Remove undefined usage
            config <- undefined
            -- config <- liftIO $
            --     Session.newEnvFromMeta meta (config args) ts (toSession env)
            let torrent = Session.sessionTorrent config
            liftIO $ Session.start config
            -- Send torrent info response
            let tname = TE.decodeUtf8 $ LBS.toStrict $ tName $ tInfo torrent
                thash = ""  -- FIXME
                tsize = fromIntegral $ torrentSize torrent
            dateAdded <- liftIO $ T.pack . formatTime defaultTimeLocale "%d-%m-%Y" <$> getCurrentTime
            json $ TorrentInfo tname thash tsize dateAdded
        post "/cancel" $
            liftIO $ atomically $ writeTBChan (toSession env) Cancel

newEnv :: IO Env
newEnv = Env <$> newTVarIO Nothing <*> newTBChanIO 32 -- TODO: Remove hardcoded value

main :: IO ()
main = do
    let opts = info (argsd <**> helper)
                ( fullDesc
               <> progDesc "BitTorrent client"
               <> Options.Applicative.header "hello, world"
                )
    args <- execParser opts
    Env ts chan <- newEnv
    config <- Dhall.inputFile Dhall.auto (configFile args)
    Session.newEnvFromMeta (torrentFile args) config ts chan
        >>= Session.start

argsd :: Parser Arguments
argsd = Arguments
    <$> strOption
        ( short 'c'
       <> value "config.dhall"
       <> metavar "SETTINGS-FILE"
       <> help "Settings file to use"
        )
    <*> strOption
        ( short 'f'
       <> metavar "TORRENT-FILE"
       <> help "Torrent file to download"
       )
