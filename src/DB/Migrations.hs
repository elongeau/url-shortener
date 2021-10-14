module DB.Migrations (runDbMigration) where

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import Control.Monad (void)
import Database.PostgreSQL.Simple (withTransaction)
import Database.PostgreSQL.Simple.Migration (runMigrations, MigrationCommand (MigrationInitialization, MigrationDirectory))

runDbMigration :: Pool.Pool Sql.Connection -> IO ()
runDbMigration pool = Pool.withResource pool \conn -> do
  void $
    withTransaction conn $
      runMigrations
        True
        conn
        [ MigrationInitialization,
          MigrationDirectory "sql"
        ]
