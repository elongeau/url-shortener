{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infra.App.Env (Env (..)) where

import Control.Monad.Reader (MonadReader)
import Core (Has (obtain), Logger, TimeProvider, UrlRepository)
import Handlers (BaseUrl)

data Env m = Env
  { envPort :: Int,
    envUrlRepository :: UrlRepository m,
    envTimeProvider :: TimeProvider m,
    envBaseUrl :: BaseUrl,
    envLogger :: Logger m
  }

instance (MonadReader (Env m) m) => Has (UrlRepository m) (Env m) where
  obtain = envUrlRepository

instance (MonadReader (Env m) m) => Has (TimeProvider m) (Env m) where
  obtain = envTimeProvider

instance (MonadReader (Env m) m) => Has BaseUrl (Env m) where
  obtain = envBaseUrl

instance (MonadReader (Env m) m) => Has (Logger m) (Env m) where
  obtain = envLogger
