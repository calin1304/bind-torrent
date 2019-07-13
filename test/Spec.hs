{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Set                as Set

import           Data.ByteString.Builder

import           Data.Binary             (decode, encode)
import           Data.Binary.Get         (runGet)
import           Net.IPv4                (ipv4)

import           Message
import qualified Peer                    as Peer
import qualified Tracker                 as Tracker

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

