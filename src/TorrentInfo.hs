module TorrentInfo
       ( TorrentInfo(..)
       , TorrentStatus(..)
       ) where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Word    (Word)
import           GHC.Generics

data TorrentInfo = TorrentInfo
    { tiName      :: Text
    , tiHash      :: Text
    , tiSize      :: Word
    , tiDateAdded :: Text
    } deriving (Generic, Show)

instance ToJSON TorrentInfo where

data TorrentStatus = TorrentStatus
    { tsDownloaded    :: Word
    , tsDownloadSpeed :: Word
    } 
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

