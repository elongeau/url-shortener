module DB.Functions where

import qualified Database.PostgreSQL.Simple as Sql
import qualified Data.Pool as Pool
import Control.Monad.State (liftIO)
import Database.PostgreSQL.Simple (ToRow)
import Data.Functor (void)
import App.Env (WithDB, DBPool, grab)

withPool :: WithDB env m => (Sql.Connection -> IO b) -> m b
withPool f = do
    pool <- grab @DBPool
    liftIO $ Pool.withResource pool f
{-# INLINE withPool #-}

executeOne :: (WithDB env m, ToRow a) => Sql.Query -> a -> m ()
executeOne query arg = withPool \conn -> void $ Sql.execute conn query arg
{-# INLINE executeOne #-}