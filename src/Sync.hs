{-# LANGUAGE OverloadedStrings #-}

module Sync (handle_sync) where

import           Control.Lens               hiding ((.=))
import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Default
import           Data.Maybe                 (fromMaybe)
import           Network.Connection
import           Network.HTTP.Conduit       hiding (responseBody,
                                             responseStatus)
import qualified Network.Wreq               as W
import qualified Network.Wreq.Types         as WT
import           System.Directory
import           System.FilePath
import           System.IO

import           Types


get_file :: FilePath -> BState BS.ByteString
get_file fp = do
  ropt <- lift request_options
  conf <- get
  case opt_url_base conf of
    Nothing -> error "Need to supply the -u option."
    Just ubase -> do
      let filename = ubase ++ fp
      rq <- lift $ W.getWith ropt filename
      return $ rq ^. W.responseBody
 where
   request_options :: IO WT.Options
   request_options = do
     lx <- newManager $ mkManagerSettings tls_settings Nothing
     return $ W.defaults { WT.manager = Right lx }
   tls_settings :: TLSSettings
   tls_settings = def { settingDisableCertificateValidation = True }

rebuild_filesystem :: Manifest -> FilePath -> BState ()
rebuild_filesystem manifest destination = do
  lift $ do
    hSetBuffering stdout NoBuffering
    putStrLn "Syncing."
  conf <- get
  _ <- mapM (handle_file (sbase conf) destination) (mf_files manifest)
  lift $ putStrLn " "
 where
   sbase :: BOptions -> String
   sbase opts = fromMaybe "out" (opt_sync_base opts)

handle_file :: FilePath -> FilePath -> BFilePath -> BState ()
handle_file outd destination (BFilePath file fhash exec) = do
  let outdir = combine outd (takeDirectory file)
      localfile = destination </> normalise file
  de <- lift $ doesFileExist localfile
  case de of
    True -> do
      check_hash fhash file outdir localfile
      return ()
    False -> do
      download_file file outdir localfile
      return ()
 where
   download_file :: FilePath -> FilePath -> FilePath -> BState ()
   download_file file outdir localfile = do
     contents <- get_file file
     lift $ createDirectoryIfMissing True outdir
     lift $ BS.writeFile localfile contents
     case exec of
       False -> lift $ putStr "+"
       True -> do
         lx <- lift $ getPermissions localfile
         lift $ setPermissions localfile (setOwnerExecutable True lx)
         lift $ putStr "*"

   check_hash :: (Maybe String) -> FilePath -> FilePath -> FilePath -> BState ()
   check_hash Nothing _ _ _ = lift $ putStr "*"
   check_hash (Just fhash) file outdir localfile = do
     h <- lift $ hash_file localfile
     case h == fhash of
       True -> lift $ putStr "-"
       False -> download_file file outdir localfile

handle_sync :: BState ()
handle_sync = do
  opts <- get
  case (,) <$> (opt_sync opts) <*> (opt_sync_base opts) of
    Nothing -> return ()
    Just (remote_url,dest_directory) -> do
      lift $ putStrLn "Fetching manifest."
      request <- lift $ W.get remote_url
      case request ^. W.responseStatus ^. W.statusCode of
        200 ->
          case decode (request ^. W.responseBody) of
            Nothing -> error "Found manifest, but can't decode it."
            Just mn -> rebuild_filesystem mn dest_directory
        b -> error $ "Response code: " ++ show b

