{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (forM_)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import qualified Data.Text                  (pack)
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.Lazy             as LT

import           Control.Lens
import           Crypto.Hash.SHA1           (hashlazy)
import           Data.BEncode
import           Data.Either.Combinators    (fromRight)
import           Data.Torrent
import           Test.RandomStrings

import           BEncoding

import           Peer                       (Peer, PeerId)
import           Tracker

randomPeerId :: IO S.ByteString
randomPeerId = return $ S.replicate 20 'A'

main :: IO ()
main = do
    peerId <- randomPeerId
    Just torrent <- bRead <$> L.readFile "hello.torrent"
    runTorrent peerId torrent

runTorrent :: PeerId -> BEncode -> IO ()
runTorrent peerId torrent = do
    let infoHash = bencodeHash $ fromJust $ lookupBDict "info" torrent
    let announce = L.unpack $ (\(BString s) -> s) $ fromJust
                   $ lookupBDict "announce" torrent
    let listenPort = 6881 --TODO: Move to separate data structure
    peers <- trackerGetPeers announce peerId infoHash listenPort
    forM_ peers print
    where
        bencodeHash :: BEncode -> S.ByteString
        bencodeHash = hashlazy . bPack

        loop :: IO ()
        loop = undefined

type Announce = L.ByteString

trackerGetPeers :: String -> PeerId -> InfoHash -> Int -> IO [Peer]
trackerGetPeers announce peerId infoHash listenPort = do
    Right response <- makeTrackerRequest $ newTrackerRequest announce infoHash
                                                   peerId listenPort
    return $ peers response


