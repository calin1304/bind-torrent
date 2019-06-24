{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as LBS

import qualified Session as Session

main :: IO ()
main = do
    [fname] <- getArgs
    LBS.readFile fname >>= Session.newConfigFromMeta >>= Session.run Session.new 
