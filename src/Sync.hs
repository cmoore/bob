{-# LANGUAGE OverloadedStrings #-}

module Sync (handle_sync) where

import           Control.Lens               hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Default
import           Data.Maybe                 (fromMaybe)
import           Network.Connection
import           Network.HTTP.Conduit       hiding (responseBody,
                                             responseStatus)
import           Network.Wreq
import qualified Network.Wreq.Types         as WT
import           System.Directory
import           System.FilePath
import           System.IO

import           Types

request_options :: IO WT.Options
request_options = do
  lx <- newManager $ mkManagerSettings tls_settings Nothing
  return $ defaults { WT.manager = Right lx }
 where
   tls_settings :: TLSSettings
   tls_settings = def { settingDisableCertificateValidation = True }

get_file :: FilePath -> IO BS.ByteString
get_file fp = do
  ropt <- request_options
  let filename = "https://tools.ivy.io/" ++ fp
  rq <- getWith ropt filename
  return $ rq ^. responseBody

rebuild_filesystem :: BOptions -> Manifest -> FilePath -> IO ()
rebuild_filesystem opts manifest destination = do
  hSetBuffering stdout NoBuffering
  putStrLn "Syncing."
  _ <- mapM (handle_file (sbase opts) destination) (mf_files manifest)
  putStrLn " "
 where
   sbase :: BOptions -> String
   sbase opts = fromMaybe "out" (opt_sync_base opts)

handle_file :: FilePath -> FilePath -> BFilePath -> IO ()
handle_file outd destination (BFilePath file fhash exec) = do
  let outdir = combine outd (takeDirectory file)
      localfile = destination </> normalise file
  de <- doesFileExist localfile
  case de of
    True -> check_hash fhash file outdir localfile
    False -> download_file file outdir localfile
 where
   download_file :: FilePath -> FilePath -> FilePath -> IO ()
   download_file file outdir localfile = do
      contents <- get_file file
      createDirectoryIfMissing True outdir
      BS.writeFile localfile contents
      case exec of
        False -> putStr "+"
        True -> do
          lx <- getPermissions localfile
          setPermissions localfile (setOwnerExecutable True lx)
          putStr "*"

   check_hash :: (Maybe String) -> FilePath -> FilePath -> FilePath -> IO ()
   check_hash Nothing _ _ _ = putStr "*"
   check_hash (Just fhash) file outdir localfile = do
     h <- hash_file localfile
     case h == fhash of
       True -> putStr "-"
       False -> download_file file outdir localfile



handle_sync :: BOptions -> IO ()
handle_sync opts =
  case (,) <$> (opt_sync opts) <*> (opt_sync_base opts) of
    Nothing -> return ()
    Just (remote_url,dest_directory) -> do
      putStrLn "Fetching manifest."
      request <- get remote_url
      case request ^. responseStatus ^. statusCode of
        200 ->
          case decode (request ^. responseBody) of
            Nothing -> error "Found manifest, but can't decode it."
            Just mn -> rebuild_filesystem opts mn dest_directory
        b -> error $ "Response code: " ++ show b
