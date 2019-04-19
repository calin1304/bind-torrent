module Peer
    ( Peer(..)
    , PeerId
    , ActivePeer
    , mkPeer
    , mkActivePeer
    ) where

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

import           Data.IP               (IPv4)

import qualified Data.ByteString.Char8 as C

type PeerId = C.ByteString

data Peer = Peer
    { maybeActive :: Maybe ActivePeer
    , ip          :: IPv4
    , port        :: Int
    } deriving(Show)

data ActivePeer = ActivePeer
    { peerId       :: PeerId
    , isInterested :: Bool
    , isChoking    :: Bool
    , amInterested :: Bool
    , amChoking    :: Bool
    } deriving(Show)

mkPeer :: IPv4 -> Int -> Peer
mkPeer = Peer Nothing

mkActivePeer :: Peer -> PeerId -> Peer
mkActivePeer peer peerId =
    peer {maybeActive = Just $ ActivePeer peerId False True False True}
