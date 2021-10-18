{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module App.Env (Env (..),  grab, Has(..)) where

import Control.Monad.Reader (asks, MonadReader)
import qualified Domain.Urls as Urls

data Env m = Env
  { envPort :: Int,
    urlService :: Urls.Service m
  }

class Has field env where
  obtain :: env -> field

instance (MonadReader (Env m) m) => Has (Urls.Service m) (Env m) where
  obtain  = urlService

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field