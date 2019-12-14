import           Conduit                 (ConduitT, runConduit, sinkList,
                                          yieldMany, (.|))
import           Control.Monad.Reader
import           Data.Binary             (decode, encode)
import           Data.ByteString.Builder
import           Test.Hspec
import           Test.QuickCheck

import           Internal.Message        (bitfieldToSet)
import           Internal.Peer
import           Message

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Set                as Set

import qualified Peer                    as Peer
import qualified Tracker                 as Tracker


main :: IO ()
main = hspec $ do
    describe "Message" $ do
        describe "decode" $
            it "is inverse of encode" $ do
                decode (encode KeepAlive)  `shouldBe` KeepAlive
                decode (encode Choke)      `shouldBe` Choke
                decode (encode Unchoke)    `shouldBe` Unchoke
                decode (encode Interested) `shouldBe` Interested
                decode (encode (Have 1))   `shouldBe` Have 1

        describe "bitfieldToSet" $
            it "works as expected" $
                Set.toList (bitfieldToSet (BS.pack [128])) `shouldBe` [0]

    describe "Peer" $
        describe "groupLength" $
            it "works as expected" $ do
                groupLength 1 5 10 `shouldBe` 5
                groupLength 1 5 6  `shouldBe` 1

--     describe "Internal.Peer" $
--         describe "handleMessages" $
--             it "works as expected" $ do
--                 env <- newMockPeerEnv
--                 let messages = yieldMany [Handshake "" ""] :: ConduitT () Message MockPeerM ()
--                     conduit = messages .| handleMessages .| sinkList
--                 l <- runReaderT (runConduit conduit) env
--                 l `shouldBe` [Handshake "" ""]

data MockPeerEnv = MockPeerEnv
    { }

type MockPeerM = ReaderT MockPeerEnv IO

newMockPeerEnv = undefined

instance HasDownloadMovingWindow MockPeerEnv where
    downloadMovingWindow = undefined

instance HasTorrentInfo MockPeerEnv where
    torrentInfo = undefined

instance HasPiecesInfo MockPeerEnv where
    requestedPiece = undefined
    remotePieces = undefined

instance HasBlocksInfo MockPeerEnv where
    requestedBlocks = undefined
    downloadedBlocks = undefined
    remainingBlocks = undefined

instance HasPiecesMgrChan MockPeerEnv where
    piecesMgrChan = undefined

instance HasInfoHash MockPeerEnv where
    infoHash _ = ""

instance HasHandshakeStatus MockPeerEnv where
    waitingHandshake = undefined

instance HasPeerState MockPeerEnv where
    peerState = undefined

instance HasLocalPieces MockPeerEnv where
    localPieces = undefined

