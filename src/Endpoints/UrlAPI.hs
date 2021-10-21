{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Endpoints.UrlAPI (server, API) where

import qualified Domain.Urls.Service as Urls
import Endpoints.Model (RequestUrl (RequestUrl, raw), ShortenedUrl (ShortenedUrl))
import Servant -- (ReqBody, ServerT, StdMethod (POST), Verb, Capture)
import Prelude hiding (log)
import Domain.Urls.Model (Url(Url, urlId), LongUrl(..))
import Domain.Urls.Service (Service(shortenUrl, findUrl))
import Domain.Has ( grab)
import qualified Data.Text as T
import qualified Domain.Urls as Urls
import qualified Data.Maybe

type Created = Verb 'POST 201

-- mettre un warning sur l'absence d'authentification: une même URL d'utilisateur différent aura quand même la même version raccourci
type API = "shorten" :> ReqBody '[JSON] RequestUrl :> Created '[JSON] ShortenedUrl
  :<|> Capture "id" T.Text :> Get '[JSON] Urls.Url

shorten :: forall env m. (Urls.UrlService env m) => RequestUrl -> m ShortenedUrl
shorten RequestUrl {..} = do
  service <- grab @(Urls.Service m)
  Url{..} <- shortenUrl service $ LongUrl raw
  return $ ShortenedUrl urlId

getShortened :: forall env m. (Urls.UrlService env m) => T.Text -> m Urls.Url 
getShortened urlId = do
  service <- grab @(Urls.Service m)
  maybeUrl <- findUrl service urlId
  pure $ Data.Maybe.fromMaybe (Urls.Url "" "") maybeUrl


server :: forall env m . (Urls.UrlService env m) => ServerT API m
server = shorten :<|> getShortened
