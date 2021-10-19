{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Endpoints.UrlAPI (server, API) where

import Control.Monad.Reader (MonadReader)
import qualified Domain.Urls.Service as Urls
import Endpoints.Model (RequestUrl (RequestUrl, raw), ShortenedUrl (ShortenedUrl))
import Servant (ReqBody, ServerT, StdMethod (POST), Verb)
import Servant.API (JSON, type (:>))
import Prelude hiding (log)
import Domain.Urls.Model (Url(Url, urlId), LongUrl(..))
import Domain.Urls.Service (Service(shortenUrl))
import Domain.Has (Has, grab)

type Created = Verb 'POST 201

-- mettre un warning sur l'absence d'authentification: une même URL d'utilisateur différent aura quand même la même version raccourci
type API = "shorten" :> ReqBody '[JSON] RequestUrl :> Created '[JSON] ShortenedUrl

shorten :: forall env m. (Urls.UrlService env m)=> RequestUrl -> m ShortenedUrl
shorten RequestUrl {..} = do
  service <- grab @(Urls.Service m)
  Url{..} <- shortenUrl service $ LongUrl raw
  return $ ShortenedUrl urlId

-- TODO faire alias pour que ce soit plus propre
server :: forall env m . (Urls.UrlService env m) => ServerT API m
server = shorten
