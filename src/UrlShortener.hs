module UrlShortener (main, runServer,runIO) where

import Control.Monad.Reader (MonadIO (liftIO))
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Proxy (Proxy), hoistServer, ServerError, err404, err409)
import Servant.Server (Server, serve)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (try)
import Data.Either.Combinators (mapLeft)
import Data.Bifunctor (first)
import Control.Monad.Error.Class (liftEither)
import Database.MongoDB (connect, access, master, auth)
import Database.MongoDB.Connection (host)
import Servant.API.Generic (toServant)
import Core (AppError(AppError), AppErrorType (NotFound, ConcurrentAccess), TimeProvider(..), AppException (unAppException))
import Infra (API, routes, AppEnv, App, Env(..), Config(..),runApp, mkUrlRepository, loadConfig)
import Handlers (BaseUrl(BaseUrl))


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
toHttpError (AppError NotFound) = err404
toHttpError (AppError ConcurrentAccess) = err409

server :: AppEnv -> Server API
server env = hoistServer (Proxy @API) (runAsHandler env) (toServant routes)

runServer :: AppEnv -> Application
runServer env = serve (Proxy @API) $ server env

runIO :: AppEnv -> IO ()
runIO env@Env{..} = run envPort $ runServer env

setup :: Config -> IO AppEnv 
setup Config{..} = do 
  pipe <- liftIO $ connect (host cfgMongoHost)
  _ <- liftIO $ access pipe master "admin" $ auth cfgMongoUser cfgMongoPassword
  let envPort = cfgPort
  let envTimeProvider = TimeProvider {
    getCurrentTimestamp = liftIO $ round . (* 1000)<$> getPOSIXTime
  }
  let envUrlRepository = mkUrlRepository pipe
  let envBaseUrl = BaseUrl cfgBaseUrl
  pure Env{..}

main :: IO ()
main = do
  conf <- loadConfig
  env <- setup conf
  putStrLn "Starting Url-Shortener app"
  runIO env
