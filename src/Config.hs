module Config (Config (..), loadConfig) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import System.Environment.MrEnv (envAsInt, envAsString)

data Config = Config
  { cfgPort :: Int,
    cfgDbCredentials :: ByteString
  }
  deriving stock (Show, Eq)

loadConfig :: IO (Maybe Config)
loadConfig = do
  dbCred <- envAsString "db_credentials" ""
  port <- envAsInt "port" 8080
  pure $
    if null dbCred
      then Nothing
      else Just $ Config port (pack dbCred)
