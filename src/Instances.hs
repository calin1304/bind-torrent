module Instances where

import           Network.Connection (Connection, connectionID)

instance Show Connection where
    show conn = "Connection"
