{-# LANGUAGE TemplateHaskell #-}

module Settings
    ( Settings
    , ClientSettings
    , listeningPort
    , blockSize
    , clientSettings
    ) where

import           Control.Lens (makeLenses)
import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)


data ClientSettings = ClientSettings
    { _listeningPort :: Int
    , _blockSize    :: Int
    }
    deriving (Generic, ToJSON, FromJSON)
makeLenses ''ClientSettings

data Settings = Settings
    { _clientSettings :: ClientSettings
    }
    deriving (Generic, ToJSON, FromJSON)
makeLenses ''Settings
