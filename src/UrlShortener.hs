module UrlShortener (main, runServer,runIO,API) where

import App.Env ( Env(..))
import App.Monad ( App, AppEnv, runApp  )
import Control.Monad.Reader (MonadIO (liftIO))
import qualified Endpoints.UrlAPI as U
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Proxy (Proxy), hoistServer, ServerT, ServerError, err404)
import Servant.Server (Server, serve)
import qualified Config as C
import qualified Core.Urls.Service as Urls
import qualified Infra.Repositories as Infra
import Core.TimeProvider (TimeProvider(TimeProvider, getCurrentTimestamp))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (try)
import Data.Either.Combinators (mapLeft)
import Data.Bifunctor (first)
import Control.Monad.Error.Class (liftEither)
import Core.Error (AppError(AppError, appErrorType), AppErrorType (NotFound), AppException (unAppException), WithError)
import Database.MongoDB (connect, access, master, auth)
import Database.MongoDB.Connection (host)

type API = U.API

runAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAsIO env app = do 
  x <- try $ runApp env app
  let y = mapLeft unAppException  x
  return y


runAsHandler :: forall a. AppEnv -> App a -> Handler a
runAsHandler env app = do 
  res <- liftIO $ runAsIO env app
  liftEither $ first toHttpError res

toHttpError :: AppError -> ServerError
toHttpError AppError {..} = case appErrorType of
  NotFound -> err404



appServer :: forall env m . (Urls.UrlService env m, WithError m) => ServerT API m
appServer = U.server

server :: AppEnv -> Server API
server env = hoistServer (Proxy @API) (runAsHandler env) appServer

runServer :: AppEnv -> Application
runServer env = serve (Proxy @API) $ server env

runIO :: AppEnv -> IO ()
runIO env@Env{..} = run envPort $ runServer env

setup :: C.Config -> IO AppEnv 
setup C.Config{..} = do 
  pipe <- liftIO $ connect (host cfgMongoHost)
  _ <- liftIO $ access pipe master "admin" $ auth cfgMongoUser cfgMongoPassword
  let envPort = cfgPort
  let envTimeProvider = TimeProvider {
    getCurrentTimestamp = liftIO $ round . (* 1000)<$> getPOSIXTime
  }
  let envUrlService = Urls.service
  let envUrlRepository = Infra.mkUrlRepository pipe
  pure Env{..}

main :: IO ()
main = do
  conf <- C.loadConfig
  env <- setup conf
  putStrLn "Starting Url-Shortener app"
  runIO env
