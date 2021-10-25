{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infra.App.Env (Env (..)) where

import Control.Monad.Reader (MonadReader)
import Core.Has (Has (obtain))
import Core.Repository (UrlRepository)
import Core.TimeProvider (TimeProvider)

data Env m = Env
  { envPort :: Int,
    envUrlRepository :: UrlRepository m,
    envTimeProvider :: TimeProvider m
  }

instance (MonadReader (Env m) m) => Has (UrlRepository m) (Env m) where
  obtain = envUrlRepository

instance (MonadReader (Env m) m) => Has (TimeProvider m) (Env m) where
  obtain = envTimeProvider
