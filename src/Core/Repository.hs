{-# LANGUAGE ConstraintKinds #-}

module Core.Repository where

import Control.Monad.Reader (MonadReader)
import Core.Has (Has)
import Core.Urls (ShortUrl, Url)

data Repository m k v = Repository
  { save :: v -> m v,
    findById :: k -> m (Maybe v)
  }

type UrlRepository m = Repository m ShortUrl Url

type WithUrlRepository env m = (MonadReader env m, Has (UrlRepository m) env)
