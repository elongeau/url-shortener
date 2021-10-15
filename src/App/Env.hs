{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module App.Env (Env (..),  grab, Has(..),WithDB, DBPool) where

import Control.Monad.Reader (asks, MonadIO, MonadReader)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql

type DBPool = Pool.Pool Sql.Connection

newtype Env  = Env
  { envPort :: Int
  }

class Has field env where
  obtain :: env -> field

type WithDB env m = (MonadReader env m, MonadIO m, Has DBPool env)

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field