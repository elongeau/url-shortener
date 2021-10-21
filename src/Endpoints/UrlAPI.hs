{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Endpoints.UrlAPI (server, API) where

import qualified Domain.Urls.Service as Urls
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
import Domain.Urls.Model (Url(Url, urlId, urlRaw), LongUrl(..))
import Domain.Urls.Service (Service(shortenUrl, findUrl))
import Domain.Has ( grab)
import qualified Data.Text as T
import qualified Domain.Urls as Urls
import qualified Data.Maybe
import qualified Data.Text.Encoding as T

type Created = Verb 'POST 201
type Redirect loc = Verb 'GET 301 '[JSON] (Headers '[Header "Location" loc] NoContent)


-- mettre un warning sur l'absence d'authentification: une même URL d'utilisateur différent aura quand même la même version raccourci
type API = "shorten" :> ReqBody '[JSON] RequestUrl :> Created '[JSON] ShortenedUrl
  :<|> Capture "id" T.Text :> Redirect UrlForHeader

shorten :: forall env m. (Urls.UrlService env m) => RequestUrl -> m ShortenedUrl
shorten RequestUrl {..} = do
  service <- grab @(Urls.Service m)
  Url{..} <- shortenUrl service $ LongUrl raw
  return $ ShortenedUrl urlId

newtype UrlForHeader = UrlForHeader Urls.Url 
instance ToHttpApiData UrlForHeader  where
  toHeader (UrlForHeader (Urls.Url raw _)) = T.encodeUtf8 raw
  toUrlPiece = undefined 

redirect :: forall env m. (Urls.UrlService env m) => T.Text -> m (Headers '[Header "Location" UrlForHeader] NoContent)
redirect urlId = do
  service <- grab @(Urls.Service m)
  maybeUrl <- findUrl service urlId
  let url = UrlForHeader $ Data.Maybe.fromMaybe (Urls.Url "" "") maybeUrl
  return (addHeader url NoContent)

server :: forall env m . (Urls.UrlService env m) => ServerT API m
server = shorten :<|> redirect
