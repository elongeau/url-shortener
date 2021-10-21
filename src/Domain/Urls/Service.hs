{-# LANGUAGE ConstraintKinds #-}

module Domain.Urls.Service (Service (..), service, UrlService, UrlRepository) where

import Control.Monad.Reader (MonadReader)
import Domain.Has (Has, grab)
import Domain.Urls.Model (LongUrl (..), Url (..))
import qualified Data.Text as T
import Domain.Repository (Repository (save, findById))

data Service m = Service
  { shortenUrl :: LongUrl -> m Url,
    findUrl :: T.Text -> m (Maybe Url)
  }

service :: (MonadReader env m, Has (UrlRepository m) env) => Service m
service = Service {shortenUrl = shortUrl, findUrl = findUrlById}

type UrlRepository m = Repository m T.Text Url

shortUrl :: forall env m. (MonadReader env m, Has (UrlRepository m) env) => LongUrl -> m Url
shortUrl LongUrl {..} = do
  repo <- grab @(UrlRepository m)
  save repo $
    Url
      { urlRaw = lgUrl,
        urlId = "nope"
      }

findUrlById:: forall env m. (MonadReader env m, Has (UrlRepository m) env) => T.Text -> m (Maybe Url)
findUrlById urlId = do 
  repo <- grab @(UrlRepository m)
  findById repo urlId


type UrlService env m = (MonadReader env m, Has (Service m) env)
