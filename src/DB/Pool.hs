module DB.Pool where

import App.Env (DBPool)
import Data.ByteString (ByteString)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql

mkPool :: ByteString -> IO DBPool
mkPool credentials = Pool.createPool (Sql.connectPostgreSQL credentials) Sql.close 10 5 10
