module Config ( Config (..), Server (..), DB (..), Port (..),loadConfig) where

import Data.ByteString (ByteString)

data Config = Config {cServer :: Server, cDB :: DB} deriving stock (Show)

newtype Server = Server {cPort :: Port} deriving stock (Show)

newtype DB = DB {dbCredentials :: ByteString} deriving stock (Show)

newtype Port = Port Int deriving stock (Show)

loadConfig :: IO Config
loadConfig = pure $ Config {
  cServer = Server $ Port 8080,
  cDB = DB ""
}