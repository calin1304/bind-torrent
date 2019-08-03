{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Set                as Set

import           Data.ByteString.Builder
import           Test.Hspec
import           Test.QuickCheck

import           Data.Binary             (decode, encode)

import qualified Peer                    as Peer
import qualified Tracker                 as Tracker

import           Message

import           Internal.Message        (bitfieldToSet)
import           Internal.Peer           (groupLength)

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
