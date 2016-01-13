{-# LANGUAGE OverloadedStrings #-}

module Manifest (handle_manifest) where

import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Directory
import           System.FilePath

import           Types


handle_manifest :: BState ()
handle_manifest = do
  opts <- get
  case (,) <$> (opt_generate opts) <*> (opt_gen_base opts) of
    Nothing -> return ()
    Just (gen,base) -> do
      lift $ putStrLn "Generating manifest from current directory."
      files <- lift $ walk_directory $ BFilePath base Nothing False
      lift $ BSL.writeFile gen $ encode $ Manifest files "0.0.1"
      return ()

walk_directory :: BFilePath -> IO [BFilePath]
walk_directory bf = do
     let the_filepath = bf_filepath bf
     names <- getDirectoryContents the_filepath
     let names' = filter (`notElem` [".", ".."]) names
     paths <- forM names' $ \name -> do
       let path = the_filepath </> name
       perms <- getPermissions path
       is_directory <- doesDirectoryExist path
       if is_directory
         then walk_directory $ BFilePath path Nothing False
         else do
         h <- hash_file path
         return [BFilePath path (Just h) (executable perms)]
     return (concat paths)
