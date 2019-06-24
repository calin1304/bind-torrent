{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Protocol where

import           Control.Lens
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import Control.Monad.Logger
import           Control.Monad.State
import           Data.Binary            (decode, encode)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text as Text
import Data.Text(Text)

-- Network imports
import qualified Net.IPv4 as IPv4
import qualified Network.Simple.TCP as TCP
import Net.IPv4(IPv4(..))

-- Library imports
import qualified Internal.Handshake     as Handshake
import qualified Internal.Message       as Message
import qualified Peer                   as Peer
import qualified Types                  as Types

import Types (InfoHash)
import           Internal.Handshake     (Handshake)
import           Internal.Message       (Message)
import           Peer                   (Peer, PeerId)

type ProtocolM a = ReaderT ProtocolConfig (LoggingT IO) a

data ProtocolConfig = ProtocolConfig 
                      { infoHash :: InfoHash
                      , peerId :: PeerId
                      }

newConfig :: InfoHash -> PeerId -> ProtocolConfig
newConfig = ProtocolConfig

run :: ProtocolM a -> ProtocolConfig -> IO a
run m conf = runStdoutLoggingT $ runReaderT m conf

new :: Peer -> ProtocolM ()
new peer = do
    peer' <- mkPeerConnection peer
    sendHandshake peer' 
    hs <- recvHandshake peer'
    ih <- asks infoHash
    unless (isValidHandshake ih hs) $ error "Invalid handshake"
    $(logDebug) (Text.concat ["Received handshake: ", Text.pack (show hs)])

mkPeerConnection :: Peer -> ProtocolM Peer
mkPeerConnection peer = do
    let peerAddr = IPv4.encodeString (Peer._ip peer)
    let peerPort = show $ Peer._port peer
    socket <- liftIO $ fst <$> TCP.connectSock peerAddr peerPort
    let ap = Peer.newActivePeer socket
    return $ peer & Peer.maybeActive ?~ ap

sendHandshake :: Peer -> ProtocolM ()
sendHandshake peer = do
    ih <- asks infoHash
    clientPeerId <- asks peerId
    $(logDebug) (Text.concat ["Sending handshake to ", Text.pack (show peer)])
    case peer ^. Peer.maybeActive of
        Just ap -> do
            let hs = LBS.toStrict $ encode $ Handshake.new ih clientPeerId
            liftIO $ TCP.send (ap ^. Peer.socket) hs
        Nothing -> error "Peer not connected" -- TODO: Connect to peer

recvHandshake :: Peer -> ProtocolM Handshake
recvHandshake peer = do
    ih <- asks infoHash
    clientPeerId <- asks peerId
    case peer ^. Peer.maybeActive of
        Just ap -> let sizeOfHandshake = 68
                    in do 
                        maybeBs <- liftIO $ TCP.recv (ap ^. Peer.socket) sizeOfHandshake
                        return $ case maybeBs of
                                    Just bs -> decode $ LBS.fromStrict bs
                                    Nothing -> error "peer disconnected"
        Nothing -> error "Peer not connected"

isValidHandshake :: Types.InfoHash -> Handshake -> Bool
isValidHandshake ih h = h ^. Handshake.infoHash == ih -- TODO: Check peer id

handleMessage :: Message -> ProtocolM ()
handleMessage Message.KeepAlive                 = $(logDebug) "KeepAlive message received"
handleMessage Message.Choke                     = $(logDebug) "Choke message received" --(\p -> p & isChoking .~ True) <$> gets _peer
handleMessage Message.Unchoke                   = $(logDebug) "Unchoke message received" --(\p -> p & isChoking .~ False) <$> gets _peer
handleMessage Message.Interested                = $(logDebug) "Interested message received" --(\p -> p & isInterested .~ True) <$> gets _peer
handleMessage Message.NotInterested             = $(logDebug) "NotInterested message received" --(\p -> p & isInterested .~ False) <$> gets _peer
handleMessage (Message.Have ix)                 = $(logDebug) "Have message received"
handleMessage (Message.BitField bf)             = $(logDebug) "BitField message received"
handleMessage (Message.Request ix begin length) = $(logDebug) "Request message received"
handleMessage (Message.Piece ix begin block)    = $(logDebug) "Piece message received"
handleMessage (Message.Cancel ix begin length)  = $(logDebug) "Cancel message received"
handleMessage (Message.Port n)                  = $(logDebug) "Port message received"
