module Infra.Routes where

import Handlers (redirect, shorten, RequestUrl, ShortenedUrl, UrlForHeader)
import Infra.App.Monad (App)
import Servant
  ( Capture,
    Header,
    Headers,
    JSON,
    NoContent (..),
    ReqBody,
    StdMethod (GET, POST),
    ToHttpApiData (..),
    Verb,
    addHeader,
    type (:>),
  )
import Servant.API.Generic (ToServantApi, type (:-))
import Servant.Server.Generic (AsServerT)
import qualified Data.Text as T
import GHC.Generics (Generic)

type API = ToServantApi UrlRoutes

type AppServer = AsServerT App

type Created = Verb 'POST 201

type Redirect loc = Verb 'GET 301 '[JSON] (Headers '[Header "Location" loc] NoContent)

data UrlRoutes route = UrlRoutes
  { _shorten :: route :- "shorten" :> ReqBody '[JSON] RequestUrl :> Created '[JSON] ShortenedUrl,
    _redirect :: route :- Capture "id" T.Text :> Redirect UrlForHeader
  }
  deriving stock (Generic)

routes :: UrlRoutes AppServer
routes =
  UrlRoutes
    { _shorten = shorten,
      _redirect = redirect
    }
