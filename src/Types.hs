{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Monad              (ap, liftM, mzero)
import           Control.Monad.State
import           Crypto.Hash
import qualified Crypto.Hash.SHA1           as SH
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Default
import qualified Data.Text                  as T
import           System.Console.GetOpt
import           System.FilePath


type BState a = StateT BOptions IO a

data BFilePath = BFilePath { bf_filepath   :: FilePath
                           , bf_hash       :: Maybe String
                           , bf_executable :: Bool }
                 deriving (Show)


data Manifest = Manifest { mf_files   :: [BFilePath]
                         , mf_version :: T.Text }
              deriving (Show)



instance ToJSON BFilePath where
  toJSON (BFilePath fp h e) = object [ "filepath" .= fp
                                     , "hash" .= h
                                     , "executable" .= e ]

instance FromJSON BFilePath where
  parseJSON (Object o) =
    BFilePath <$> o .: "filepath"
              <*> o .: "hash"
              <*> o .: "executable"
  parseJSON _ = mzero

instance ToJSON Manifest where
  toJSON (Manifest f v) = object [ "files" .= f
                                 , "version" .= v ]

instance FromJSON Manifest where
  parseJSON (Object o) =
    Manifest <$> o .: "files"
             <*> o .: "version"
  parseJSON _ = mzero



data BOptions = BOptions { opt_verbose   :: Bool
                         , opt_generate  :: Maybe FilePath
                         , opt_gen_base  :: Maybe FilePath
                         , opt_sync      :: Maybe String
                         , opt_sync_base :: Maybe FilePath
                         , opt_url_base  :: Maybe String }
                deriving (Show)

instance Default BOptions where
  def = BOptions False Nothing Nothing Nothing Nothing Nothing

cmd_options :: [OptDescr (BOptions -> BOptions)]
cmd_options =
  [ Option ['g'] ["generate"]  (ReqArg gen_opt "FILE")      "generate a manifest"
  , Option ['b'] ["gen-base"]  (ReqArg gbase_opt "DIR")     "base directory for manifest"
  , Option ['s'] ["sync"]      (ReqArg sync_opt "URL")      "synchronize the given url"
  , Option ['u'] ["url"]       (ReqArg url_base_opt "URL")  "base url for file requests"
  , Option ['o'] ["sync-base"] (ReqArg sync_base_opt "DIR") "directory to synchronize to" ]
 where
   gen_opt f o =  o { opt_generate = Just f }
   gbase_opt f o = o { opt_gen_base = Just f }
   sync_opt f o = o { opt_sync = Just f }
   sync_base_opt f o = o { opt_sync_base = Just f }
   url_base_opt f o = o { opt_url_base = Just f }

bob_options :: [String] -> IO BOptions
bob_options argv =
  case getOpt Permute cmd_options argv of
    (g,_,[]) -> return $ foldl (flip id) def g
    (_,_,err) -> ioError (userError (concat err ++ usageInfo header cmd_options))
 where
   header :: String
   header = "Usage: bob [OPTION..] files..."

hash_file :: FilePath -> IO String
hash_file fp = do
  contents <- BSL.readFile fp
  return $ BS.unpack $ digestToHexByteString $ sha1 contents
 where
   sha1 :: BSL.ByteString -> Digest SHA1
   sha1 = hashlazy
