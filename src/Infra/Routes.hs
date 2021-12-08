module Infra.Routes (routes, API, UrlRoutes(..)) where

import Core (ShortUrl)
import GHC.Generics (Generic)
import Handlers (RequestUrl, ShortenedUrl, UrlForHeader, redirect, shorten)
import Infra.App.Monad (App)
import Servant
  ( Capture,
    Header,
    Headers,
    JSON,
    NoContent (..),
    ReqBody,
    StdMethod (GET, POST),
    Verb,
    type (:>),
  )
import Servant.API.Generic (ToServantApi, type (:-))
import Servant.Server.Generic (AsServerT)

type API = ToServantApi UrlRoutes

-- | Describe a 'Created' status code
type Created = Verb 'POST 201

-- | Describe a 'Redirect' status code with Location header
type Redirect loc = Verb 'GET 301 '[JSON] (Headers '[Header "Location" loc] NoContent)

-- | Define the routes of the application
data UrlRoutes route = UrlRoutes
  { postShorten :: route :- "shorten" :> ReqBody '[JSON] RequestUrl :> Created '[JSON] ShortenedUrl,
    routeRedirect :: route :- Capture "id" ShortUrl :> Redirect UrlForHeader
  }
  deriving stock (Generic)

type AppServer = AsServerT App

-- | bind routes and handlers
routes :: UrlRoutes AppServer
routes =
  UrlRoutes
    { postShorten = shorten,
      routeRedirect = redirect
    }
