module Infra.Routes where

import Servant.API.Generic (ToServantApi)
import Handlers (UrlRoutes(..), shorten, redirect)
import Servant.Server.Generic (AsServerT)
import Infra.App.Monad (App)
type API = ToServantApi UrlRoutes
type AppServer = AsServerT App

routes :: UrlRoutes AppServer
routes =
  UrlRoutes
    { _shorten = shorten,
      _redirect = redirect
    }
