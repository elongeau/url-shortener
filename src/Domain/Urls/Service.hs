{-# LANGUAGE ConstraintKinds #-}

module Domain.Urls.Service (Service (..), service, UrlService, UrlRepository, toBase62) where

import Control.Monad.Reader (MonadReader)
import Domain.Has (Has, grab)
import Domain.Urls.Model (LongUrl (..), Url (..))
import qualified Data.Text as T
import Domain.Repository (Repository (save, findById))
import Domain.TimeProvider (TimeProvider (getCurrentTimestamp))

data Service m = Service
  { shortenUrl :: LongUrl -> m Url,
    findUrl :: T.Text -> m (Maybe Url)
  }

type UrlRepository m = Repository m T.Text Url

service :: (MonadReader env m, Has (UrlRepository m) env, Has (TimeProvider m) env) => Service m
service = Service {shortenUrl = shortUrl, findUrl = findUrlById}

shortUrl :: forall env m. (MonadReader env m, Has (UrlRepository m) env, Has (TimeProvider m) env) => LongUrl -> m Url
shortUrl LongUrl {..} = do
  repo <- grab @(UrlRepository m)
  timestamp <- grab @(TimeProvider m) >>= getCurrentTimestamp
  let urlId = toBase62 timestamp
  save repo $
    Url
      { urlRaw = lgUrl,
        urlId = urlId
      }

findUrlById:: forall env m. (MonadReader env m, Has (UrlRepository m) env) => T.Text -> m (Maybe Url)
findUrlById urlId = do 
  repo <- grab @(UrlRepository m)
  findById repo urlId

toBase62 :: Int -> T.Text 
toBase62 x = 
  if x == 0 then ""
  else let v = x `mod` 62
           c = T.index characters v
       in T.snoc (toBase62 (x `div` 62))  c
  where
    characters :: T.Text
    characters = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


type UrlService env m = (MonadReader env m, Has (Service m) env)
