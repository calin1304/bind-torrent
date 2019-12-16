{-# LANGUAGE TemplateHaskell #-}

module Settings
    ( Settings
    , ClientSettings
    , listeningPort
    , blockSize
    , clientSettings
    , chanCapacity
    ) where

import           Control.Lens (makeLenses)
import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)


data ClientSettings = ClientSettings
    { _listeningPort  :: Int
    , _blockSize      :: Int
    , _chanCapacity :: Int
    }
    deriving (Generic, ToJSON, FromJSON)
makeLenses ''ClientSettings

data Settings = Settings
    { _clientSettings :: ClientSettings
    }
    deriving (Generic, ToJSON, FromJSON)
makeLenses ''Settings
