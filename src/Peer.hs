{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Peer where

import           Control.Lens
import qualified Data.ByteString.Char8 as C
import           Net.IPv4              (IPv4)
import           Network.Connection    (Connection)
import           Network.Socket        (PortNumber, Socket)

import           Internal.Instances

type PeerId = C.ByteString

data ActivePeer = ActivePeer
    { _peerId       :: PeerId
    , _isInterested :: Bool
    , _isChoking    :: Bool
    , _amInterested :: Bool
    , _amChoking    :: Bool
    , _socket       :: Socket
    } deriving(Eq, Show)
makeLenses ''ActivePeer

data Peer = Peer
    { _maybeActive :: Maybe ActivePeer
    , _ip          :: IPv4
    , _port        :: PortNumber
    } deriving(Eq, Show)
makeLenses ''Peer

new :: IPv4 -> PortNumber -> Peer
new = Peer Nothing

newActivePeer :: Socket -> ActivePeer
newActivePeer = ActivePeer "" False True False True

setAmChoking :: Peer -> Bool -> Peer
setAmChoking peer b = let Just ap = peer ^. Peer.maybeActive
                        in peer { Peer._maybeActive = Just (ap { Peer._amChoking = b }) }

setAmInterested :: Peer -> Bool -> Peer
setAmInterested p b = let ma = case _maybeActive p of
                                   Nothing -> error "No active peer"
                                   Just x  -> x {_amInterested = b}
                       in p { _maybeActive = Just ma }

