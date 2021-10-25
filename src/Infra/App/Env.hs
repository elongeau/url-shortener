{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infra.App.Env (Env (..)) where

import Control.Monad.Reader (MonadReader)
import Endpoints.Model (BaseUrl)
import Core (UrlRepository, TimeProvider, Has (obtain))


data Env m = Env
  { envPort :: Int,
    envUrlRepository :: UrlRepository m,
    envTimeProvider :: TimeProvider m,
    envBaseUrl :: BaseUrl
  }

instance (MonadReader (Env m) m) => Has (UrlRepository m) (Env m) where
  obtain = envUrlRepository

instance (MonadReader (Env m) m) => Has (TimeProvider m) (Env m) where
  obtain = envTimeProvider

instance (MonadReader (Env m) m) => Has BaseUrl (Env m) where
  obtain = envBaseUrl
