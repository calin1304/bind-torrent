{-# LANGUAGE TemplateHaskell #-}

module Config
    ( Config
    , ClientConfig
    , listeningPort
    , blockSize
    , clientConfig
    , chanCapacity
    ) where

import           Control.Lens (makeLenses)
import           Data.Aeson   (FromJSON, ToJSON)
import           Dhall        (Interpret, Natural)
import           GHC.Generics (Generic)


-- TODO: Change Natural to Int
data ClientConfig = ClientConfig
    { _listeningPort :: Natural
    , _blockSize     :: Natural
    , _chanCapacity  :: Natural
    }
    deriving (Generic, ToJSON, FromJSON)
makeLenses ''ClientConfig

instance Interpret ClientConfig

newtype Config = Config
    { _clientConfig :: ClientConfig
    }
    deriving (Generic, ToJSON, FromJSON)
makeLenses ''Config

instance Interpret Config
