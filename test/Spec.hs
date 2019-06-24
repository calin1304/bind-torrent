{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import Data.Binary (encode, decode)
import Data.Binary.Get (runGet)
import Net.IPv4 (ipv4)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.ByteString.Builder

import Internal.Message
import qualified Tracker as Tracker
import qualified Peer as Peer
import qualified Internal.Handshake as Handshake

localhost = ipv4 127 0 0 1

main :: IO ()
main = hspec $ do
    describe "Message" $
        describe "decode" $
            it "is inverse to encode" $ do
                decode (encode KeepAlive) `shouldBe` KeepAlive
                decode (encode Choke) `shouldBe` Choke
                decode (encode Unchoke) `shouldBe` Unchoke
                decode (encode Interested) `shouldBe` Interested
                decode (encode (Have 1)) `shouldBe` Have 1

    describe "Tracker" $
        describe "getCompactPeer" $
            it "decodes peer as expected" $ do
               runGet Tracker.getCompactPeer "\x7f\x00\x00\x01\xf3\xdb" 
                    `shouldBe` Peer.new localhost 62427
               runGet Tracker.getCompactPeer "\x7f\x00\x00\x01\x1a\xe1" 
                    `shouldBe` Peer.new localhost 6881

    describe "Handshake" $
        let ih = "deadbeefdeadbeefdead"
            pi = "00112233445566778899"
            hs = toLazyByteString $ int8 19 <> string8 "BitTorrent protocol" <> reserved 
                            <> byteString ih <> byteString pi
            reserved = byteString $ BS.replicate 8 0
         in do
            describe "encode" $
                it  "works as expected" $
                    encode (Handshake.new ih pi) `shouldBe` hs
            describe  "decode" $
                it "works as expected" $
                    decode hs `shouldBe` Handshake.new ih pi
