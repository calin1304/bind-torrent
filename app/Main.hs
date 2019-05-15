{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Session (runSessionFromFile)

main :: IO ()
main = runSessionFromFile "hello.torrent"