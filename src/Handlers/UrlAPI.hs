{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.UrlAPI (UrlRoutes(..), shorten, redirect) where

import Handlers.Model (RequestUrl (RequestUrl, raw), ShortenedUrl (ShortenedUrl), BaseUrl(..))
import Servant
    ( addHeader,
      ToHttpApiData(..),
      StdMethod(POST, GET),
      Capture,
      JSON,
      NoContent(..),
      Header,
      ReqBody,
      Headers,
      type (:>),
      Verb )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Servant.API.Generic (type (:-))
import GHC.Generics (Generic)
import Core (WithError, WithTimeProvider, WithUrlRepository, Has, UrlRepository, TimeProvider (getCurrentTimestamp), Url(..), grab, Repository (findById, save), throwError, AppErrorType (NotFound, ConcurrentAccess), shortenUrl, LongUrl(..))

type Created = Verb 'POST 201
type Redirect loc = Verb 'GET 301 '[JSON] (Headers '[Header "Location" loc] NoContent)

data UrlRoutes route = UrlRoutes {
  _shorten :: route :- "shorten" :> ReqBody '[JSON] RequestUrl :> Created '[JSON] ShortenedUrl,
  _redirect :: route :- Capture "id" T.Text :> Redirect UrlForHeader 
} deriving stock Generic

shorten :: forall env m. (WithError m, WithTimeProvider env m, WithUrlRepository env m, Has BaseUrl env) => RequestUrl -> m ShortenedUrl
shorten RequestUrl {..} = go 3 -- tries 3 times before giving up
  where 
    go :: Int -> m ShortenedUrl
    go 0 = throwError ConcurrentAccess
    go n = do
      save' <- save <$> grab @(UrlRepository m)
      findById' <- findById <$> grab @(UrlRepository m)
      baseUrl <- base <$> grab @BaseUrl
      timestamp <- grab @(TimeProvider m) >>= getCurrentTimestamp
      let url@Url{..} = shortenUrl timestamp $ LongUrl raw
      maybeAlreadyExists <- findById' urlId
      case maybeAlreadyExists of
        Nothing -> do 
          _ <- save' url
          pure $ ShortenedUrl $ baseUrl <> "/" <> urlId 
        Just _ -> go $ n - 1

newtype UrlForHeader = UrlForHeader Url 
instance ToHttpApiData UrlForHeader  where
  toHeader (UrlForHeader (Url raw _)) = T.encodeUtf8 raw
  toUrlPiece = undefined -- not used

redirect :: forall env m. (WithError m, WithUrlRepository env m) => T.Text -> m (Headers '[Header "Location" UrlForHeader] NoContent)
redirect urlId = do
  repo <- grab @(UrlRepository m)
  maybeUrl <- findById repo urlId
  case maybeUrl of
    Nothing -> throwError NotFound 
    Just url -> return (addHeader (UrlForHeader url) NoContent)
