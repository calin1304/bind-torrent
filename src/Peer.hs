{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Peer where

{-
    Client maintains state of each connection with remote peer
        - choked
            whether remote peer choked client, no request will be anwered until
            unchoked.
        - interested
            remote peer is interested in something the client has to offer.
            remote peer will request blocks when it gets unchoked
    Client connections start choked and not interested.
    A block is downloaded when
        - client interested in block
        - remote not choking client
    A block is uploaded when
        - remote interested in block
        - client not choking remote

Data Types
    - all integers are 4 byte big endian.

Message flow

    Initial handshake:
        <pstrlen><pstr><reserved><info_hash><peer_id>
        pstrlen = 19
        pstr = "BitTorrent protocol"

    Messages
        <length prefix><message ID><payload>
        types: keep-alive, choke, unchoke, interested, not interested, have,
               bitfield, request, piece, cancel, port

-}

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
    , _socket   :: Socket
    } deriving(Show)
makeLenses ''ActivePeer

instance Eq ActivePeer where
    (==) a b = all (\x -> x a == x b) [_isInterested, _isChoking, _amInterested, _amChoking] 
                && _peerId a == _peerId b

data Peer = Peer
    { _maybeActive :: Maybe ActivePeer
    , _ip          :: IPv4
    , _port        :: PortNumber
    } deriving(Show, Eq)
makeLenses ''Peer

new :: IPv4 -> PortNumber -> Peer
new = Peer Nothing

newActivePeer :: Socket -> ActivePeer
newActivePeer = ActivePeer "" False True False True

setAmChoking :: Peer -> Bool -> Peer
setAmChoking peer b = let Just ap = peer ^. Peer.maybeActive
                        in peer { Peer._maybeActive = Just (ap { Peer._amChoking = b }) }
                        -- TODO: Rewrite with lens setter

setAmInterested :: Peer -> Bool -> Peer
setAmInterested = undefined
