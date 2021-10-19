{-# LANGUAGE ConstraintKinds #-}

module Domain.Urls.Service (Service (..), service, UrlService) where

import Control.Monad.Reader (MonadReader)
import Domain.Has (Has)
import Domain.Urls.Model (LongUrl (..), Url (..))

newtype Service m = Service
  { shortenUrl :: LongUrl -> m Url
  }

service :: (MonadReader env m) => Service m
service = Service {shortenUrl = shortUrl}

shortUrl :: (MonadReader env m) => LongUrl -> m Url
shortUrl LongUrl {..} =
  pure $
    Url
      { urlRaw = lgUrl,
        urlId = "nope"
      }

type UrlService env m = (MonadReader env m, Has (Service m) env)
