{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BS8

import Console

main :: IO ()
main = do
  b <- BS.readFile "/Users/phartig/Downloads/cute-unicorn-clipart-unicorn4.png"
  render <- getImageRenderer
  BS8.putStrLn . render $ consoleImage True b
