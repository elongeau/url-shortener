{-# LANGUAGE ConstraintKinds #-}

module Domain.Urls.Service (Service (..), service, UrlService, UrlRepository) where

import Control.Monad.Reader (MonadReader)
import Domain.Has (Has, grab)
import Domain.Urls.Model (LongUrl (..), Url (..))
import qualified Data.Text as T
import Domain.Repository (Repository (save))

newtype Service m = Service
  { shortenUrl :: LongUrl -> m Url
  }

service :: (MonadReader env m, Has (UrlRepository m) env) => Service m
service = Service {shortenUrl = shortUrl}

type UrlRepository m = Repository m T.Text Url

shortUrl :: forall env m. (MonadReader env m, Has (UrlRepository m) env) => LongUrl -> m Url
shortUrl LongUrl {..} = do
  repo <- grab @(UrlRepository m)
  save repo $
    Url
      { urlRaw = lgUrl,
        urlId = "nope"
      }

type UrlService env m = (MonadReader env m, Has (Service m) env)
