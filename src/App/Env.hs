{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module App.Env (Env (..),  grab, Has(..),WithDB, DBPool) where

import Control.Monad.Reader (asks, MonadIO, MonadReader)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql

type DBPool = Pool.Pool Sql.Connection

data Env  = Env
  { envPort :: !Int,
    envDbPool :: !DBPool
  }

class Has field env where
  obtain :: env -> field

instance Has DBPool Env where obtain = envDbPool
type WithDB env m = (MonadReader env m, MonadIO m, Has DBPool env)

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field