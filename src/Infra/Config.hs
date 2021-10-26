module Infra.Config (Config (..), loadConfig) where

import qualified Data.Text as T
import System.Environment.MrEnv (envAsInt, envAsString)

data Config = Config
  { cfgPort :: Int,
    cfgMongoHost :: String,
    cfgMongoUser :: T.Text,
    cfgMongoPassword :: T.Text,
    cfgHostUrl :: T.Text
  }
  deriving stock (Show, Eq)

loadConfig :: IO Config
loadConfig = do
  cfgPort <- envAsInt "SERVER_PORT" 8080
  cfgMongoHost <- envAsString "MONGO_HOST" "localhost"
  cfgMongoUser <- T.pack <$> envAsString "MONGO_USER" "root"
  cfgMongoPassword <- T.pack <$> envAsString "MONGO_PASSWORD" "password"
  cfgHostUrl <- T.pack <$> envAsString "BASE_URL" "http://localhost:8080"
  pure Config {..}
