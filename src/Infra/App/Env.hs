{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infra.App.Env (Env (..), DbPool) where

import Control.Monad.Reader (MonadReader)
import Core (Has (obtain), Logger, TimeProvider, UrlRepository)
import Data.Pool (Pool)
import Database.MongoDB (Pipe)
import Handlers (HostUrl)

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
