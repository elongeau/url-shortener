{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Endpoints.UrlAPI (server,  API) where

import App.Env (WithDB)
import Servant (ReqBody, ServerT)
import Servant.API (JSON, Post, type (:>))
import Prelude hiding (log)
import Endpoints.Model (RequestUrl, ShortenedUrl(ShortenedUrl))

-- mettre un warning sur l'absence d'authentification: une même URL d'utilisateur différent aura quand même la même version raccourci
type API = "shorten" :> ReqBody '[JSON] RequestUrl :> Post '[JSON] ShortenedUrl

shorten :: (Monad m) => RequestUrl -> m ShortenedUrl
shorten _ = pure $ ShortenedUrl "nope"

server :: WithDB env m => ServerT API m
server = shorten
