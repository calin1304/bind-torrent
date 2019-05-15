{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import           Control.Lens
import           Control.Monad        (when)
import           Control.Monad.Reader (asks)
import           Data.Binary          (encode)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Network.Simple.TCP   (SockAddr, Socket, connectSock, send)

import           Internal.Handshake   hiding (_infoHash, _peerId)
import           Internal.Message
import           Peer                 (ActivePeer, Peer, PeerId, maybeActive,
                                       mkActivePeer, socket)
import           Session

mkPeerConnection :: Peer -> IO Peer
mkPeerConnection = undefined
-- mkPeerConnection (Peer _ ip port) = do
--     (socket, sockaddr) <- connectSock (show ip) (show port)
--     return $ peer {} PeerConnection socket sockaddr

sessionPeerId :: BS.ByteString
sessionPeerId = "AAAAAAAAAAAAAAAAAAAAA"

sendHandshake :: Peer -> Session ()
sendHandshake peer = do
    ih <- asks _infoHash
    clientPeerId <- asks _peerId
    case peer ^. maybeActive of
        Just ap -> do
            let hs = LBS.toStrict $ encode $ mkHandshake ih clientPeerId
            send (ap ^. socket) hs
        Nothing -> error "Peer not connected" -- TODO: Connect to peer

recvHandshake :: Peer -> IO Handshake
recvHandshake = undefined

openPeer :: Peer -> IO (Either String Peer)
openPeer = undefined
-- openPeer peer = do
--     conn <- mkPeerConnection peer
--     sendHandshake conn
--     h <- recvHandshake conn
--     return $ if isValidHandshake h
--                 then Right $ mkActivePeer peer $ peerId h
--                 else Left "Invalid handshake received"

sendMessage :: Message -> Peer -> IO ()
sendMessage peer = undefined

recvMessage :: Peer -> IO Message
recvMessage peer = undefined

setAmChoking :: Peer -> Bool -> Peer
setAmChoking = undefined

setAmInterested :: Peer -> Bool -> Peer
setAmInterested = undefined

handleMessages :: Peer -> IO ()
handleMessages = undefined

downloadPieces :: Peer -> IO ()
downloadPieces = undefined

runPeerProtocol :: Peer -> BS.ByteString -> IO ()
runPeerProtocol = undefined
-- runPeerProtocol peer infoHash = do
--     sendHandshake peer infoHash
--     h <- recvHandshake peer
--     when (isValidHandshake h infoHash) $ do
--         -- setAmChoking peer False
--         -- setAmInterested peer True
--         handleMessages peer
--         downloadPieces peer
