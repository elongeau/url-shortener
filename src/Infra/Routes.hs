module Infra.Routes (routes, API) where

import qualified Data.Text as T
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
  { _shorten :: route :- "shorten" :> ReqBody '[JSON] RequestUrl :> Created '[JSON] ShortenedUrl,
    _redirect :: route :- Capture "id" T.Text :> Redirect UrlForHeader
  }
  deriving stock (Generic)

type AppServer = AsServerT App

-- | bind routes and handlers
routes :: UrlRoutes AppServer
routes =
  UrlRoutes
    { _shorten = shorten,
      _redirect = redirect
    }
