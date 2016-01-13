
module Main where

import           Control.Monad.State
import           System.Console.GetOpt
import           System.Environment

import           Manifest
import           Sync
import           Types

main :: IO ()
main = do
  options <- getArgs >>= bob_options
  runStateT (handle_manifest >> handle_sync) options
  return ()
