{-# LANGUAGE ConstraintKinds #-}

module Core.Repository where

import Control.Monad.Reader (MonadReader)
import Core.Has (Has)
import Core.Urls (Url)
import qualified Data.Text as T

data Repository m k v = Repository
  { save :: v -> m v,
    findById :: k -> m (Maybe v)
  }

type UrlRepository m = Repository m T.Text Url

type WithUrlRepository env m = (MonadReader env m, Has (UrlRepository m) env)
