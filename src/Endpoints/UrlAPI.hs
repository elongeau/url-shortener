{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Endpoints.UrlAPI (server, API) where

import qualified Core.Urls.Service as Urls
import Endpoints.Model (RequestUrl (RequestUrl, raw), ShortenedUrl (ShortenedUrl))
import Servant
    ( addHeader,
      ToHttpApiData(..),
      StdMethod(POST, GET),
      type (:<|>)(..),
      Capture,
      JSON,
      NoContent(..),
      Header,
      ReqBody,
      Headers,
      type (:>),
      Verb,
      HasServer(ServerT) )
import Core.Urls.Model (Url(Url, urlId, urlRaw), LongUrl(..))
import Core.Has ( grab)
import qualified Data.Text as T
import qualified Core.Urls as Urls
import qualified Data.Text.Encoding as T
import Core.Error (WithError, throwError, AppErrorType (NotFound, ConcurrentAccess))
import Core.TimeProvider (TimeProvider (getCurrentTimestamp), WithTimeProvider)
import Core.Repository (UrlRepository, Repository (findById, save), WithUrlRepository)

type Created = Verb 'POST 201
type Redirect loc = Verb 'GET 301 '[JSON] (Headers '[Header "Location" loc] NoContent)

type API = "shorten" :> ReqBody '[JSON] RequestUrl :> Created '[JSON] ShortenedUrl
  :<|> Capture "id" T.Text :> Redirect UrlForHeader

shorten :: forall env m. (WithError m, WithTimeProvider env m, WithUrlRepository env m) => RequestUrl -> m ShortenedUrl
shorten RequestUrl {..} = go 3 -- tries 3 times before giving up
  where 
    go :: Int -> m ShortenedUrl
    go 0 = throwError ConcurrentAccess
    go n = do
      repo <- grab @(UrlRepository m)
      timestamp <- grab @(TimeProvider m) >>= getCurrentTimestamp
      let url@Url{..} = Urls.shortenUrl timestamp $ LongUrl raw
      maybeAlreadyExists <- findById repo urlId
      case maybeAlreadyExists of
        Nothing -> do 
          _ <- save repo url
          pure $ ShortenedUrl urlId 
        Just _ -> go $ n - 1

newtype UrlForHeader = UrlForHeader Urls.Url 
instance ToHttpApiData UrlForHeader  where
  toHeader (UrlForHeader (Urls.Url raw _)) = T.encodeUtf8 raw
  toUrlPiece = undefined -- not used

redirect :: forall env m. (WithError m, WithUrlRepository env m) => T.Text -> m (Headers '[Header "Location" UrlForHeader] NoContent)
redirect urlId = do
  repo <- grab @(UrlRepository m)
  maybeUrl <- findById repo urlId
  case maybeUrl of
    Nothing -> throwError NotFound 
    Just url -> return (addHeader (UrlForHeader url) NoContent)

server :: forall env m . (WithError m, WithUrlRepository env m, WithTimeProvider env m) => ServerT API m
server = shorten :<|> redirect
