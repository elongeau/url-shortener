{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module App.Env (Env (..),  grab, Has(..)) where

import Control.Monad.Reader (asks, MonadReader)

newtype Env  = Env
  { envPort :: Int
  }

class Has field env where
  obtain :: env -> field

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field