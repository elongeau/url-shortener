{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Env (Env (..)) where

import Control.Monad.Reader (MonadReader)
import Core.Has (Has (obtain))
import Core.TimeProvider (TimeProvider)
import qualified Core.Urls as Urls

data Env m = Env
  { envPort :: Int,
    envUrlService :: Urls.Service m,
    envUrlRepository :: Urls.UrlRepository m,
    envTimeProvider :: TimeProvider m
  }

instance (MonadReader (Env m) m) => Has (Urls.UrlRepository m) (Env m) where
  obtain = envUrlRepository

instance (MonadReader (Env m) m) => Has (Urls.Service m) (Env m) where
  obtain = envUrlService

instance (MonadReader (Env m) m) => Has (TimeProvider m) (Env m) where
  obtain = envTimeProvider
