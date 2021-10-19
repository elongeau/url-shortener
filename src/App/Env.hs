{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Env (Env (..)) where

import Control.Monad.Reader (MonadReader)
import Domain.Has (Has (obtain))
import qualified Domain.Urls as Urls

data Env m = Env
  { envPort :: Int,
    urlService :: Urls.Service m
  }

instance (MonadReader (Env m) m) => Has (Urls.Service m) (Env m) where
  obtain = urlService
