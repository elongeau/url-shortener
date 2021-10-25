module Infra.Routes where

import Endpoints (UrlRoutes (..), redirect, shorten)
import Infra.App.Monad (App)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)

type API = ToServantApi UrlRoutes

type AppServer = AsServerT App

routes :: UrlRoutes AppServer
routes =
  UrlRoutes
    { _shorten = shorten,
      _redirect = redirect
    }
