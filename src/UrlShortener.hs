module UrlShortener (main, runServer,runIO) where

import Control.Monad.Reader (MonadIO (liftIO))
import qualified Endpoints.UrlAPI as U
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Proxy (Proxy), hoistServer, ServerError, err404, err409)
import Servant.Server (Server, serve)
import Core.TimeProvider (TimeProvider(TimeProvider, getCurrentTimestamp))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (try)
import Data.Either.Combinators (mapLeft)
import Data.Bifunctor (first)
import Control.Monad.Error.Class (liftEither)
import Core.Error (AppError(AppError, appErrorType), AppErrorType (NotFound, ConcurrentAccess), AppException (unAppException))
import Database.MongoDB (connect, access, master, auth)
import Database.MongoDB.Connection (host)
import qualified Infra as I
import qualified Infra.App.Monad as I
import Servant.API.Generic (toServant)


runAsIO :: I.AppEnv -> I.App a -> IO (Either AppError a)
runAsIO env app = do 
  x <- try $ I.runApp env app
  let y = mapLeft unAppException  x
  return y


runAsHandler :: forall a. I.AppEnv -> I.App a -> Handler a
runAsHandler env app = do 
  res <- liftIO $ runAsIO env app
  liftEither $ first toHttpError res

toHttpError :: AppError -> ServerError
toHttpError AppError {..} = case appErrorType of
  NotFound -> err404
  ConcurrentAccess ->   err409

server :: I.AppEnv -> Server U.API
server env = hoistServer (Proxy @U.API) (runAsHandler env) (toServant U.routes)

runServer :: I.AppEnv -> Application
runServer env = serve (Proxy @U.API) $ server env

runIO :: I.AppEnv -> IO ()
runIO env@I.Env{..} = run envPort $ runServer env

setup :: I.Config -> IO I.AppEnv 
setup I.Config{..} = do 
  pipe <- liftIO $ connect (host cfgMongoHost)
  _ <- liftIO $ access pipe master "admin" $ auth cfgMongoUser cfgMongoPassword
  let envPort = cfgPort
  let envTimeProvider = TimeProvider {
    getCurrentTimestamp = liftIO $ round . (* 1000)<$> getPOSIXTime
  }
  let envUrlRepository = I.mkUrlRepository pipe
  pure I.Env{..}

main :: IO ()
main = do
  conf <- I.loadConfig
  env <- setup conf
  putStrLn "Starting Url-Shortener app"
  runIO env
