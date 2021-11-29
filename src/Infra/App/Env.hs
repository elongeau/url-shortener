{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infra.App.Env (Env (..), withPool, DbPool) where

import Control.Monad.Reader (MonadReader,MonadIO, liftIO)
import Core (Has (obtain), Logger, TimeProvider, UrlRepository,grab)
import Handlers (HostUrl)
import Database.MongoDB (Pipe)
import qualified Data.Pool as Pool
import Data.Pool (Pool)

type DbPool = Pool Pipe

data Env m = Env
  { envPort :: Int,
    envUrlRepository :: UrlRepository m,
    envTimeProvider :: TimeProvider m,
    envHostUrl :: HostUrl,
    envLogger :: Logger m,
    envDB :: DbPool
  }

instance (MonadReader (Env m) m) => Has (UrlRepository m) (Env m) where
  obtain = envUrlRepository

instance (MonadReader (Env m) m) => Has (TimeProvider m) (Env m) where
  obtain = envTimeProvider

instance (MonadReader (Env m) m) => Has HostUrl (Env m) where
  obtain = envHostUrl

instance (MonadReader (Env m) m) => Has (Logger m) (Env m) where
  obtain = envLogger

type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

withPool :: WithDb env m => (Pipe -> IO b) -> m b
withPool f = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool f
