
module Main where

import           System.Console.GetOpt
import           System.Environment

import           Manifest
import           Sync
import           Types

main :: IO ()
main = do
  options <- getArgs >>= bob_options
  handle_manifest options
  handle_sync options
