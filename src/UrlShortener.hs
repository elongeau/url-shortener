module UrlShortener (runIO, runAsApplication, mkAppEnv,runAsIO) where

import Control.Monad.Reader (MonadIO (liftIO))
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Proxy (Proxy), hoistServer, ServerError (errBody), err404, err409, err400)
import Servant.Server (Server, serve)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (try)
import Data.Either.Combinators (mapLeft)
import Data.Bifunctor (first)
import Control.Monad.Error.Class (liftEither)
import Database.MongoDB (connect, access, master, auth,close, Pipe)
import Database.MongoDB.Connection (host)
import Servant.API.Generic (toServant)
import Core (AppError(AppError), AppErrorType (NotFound, ConcurrentAccess, NotAnUrl), TimeProvider(..), AppException (unAppException))
import Infra (API, routes, AppEnv, App, Env(..), Config(..),runApp, mkUrlRepository, loadConfig, consoleLogger)
import Handlers (HostUrl(HostUrl))
import qualified Data.Pool as Pool

-- | entry point for the application
runIO :: IO ()
runIO = loadConfig >>= mkAppEnv >>= runAsIO

-- | Setup everything necessary for 'Env'
mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    envDB <- liftIO initialisePool
    let envPort = cfgPort
    let envTimeProvider = TimeProvider {
      getCurrentTimestamp = liftIO $ round . (* 1000)<$> getPOSIXTime
    }
    let envUrlRepository = mkUrlRepository envDB
    let envHostUrl = HostUrl cfgHostUrl
    envLogger <- consoleLogger
    pure Env{..}
  where
    initialisePool = Pool.createPool connectDB close 10 5 10
    connectDB :: IO Pipe
    connectDB = do
      pipe <- connect (host cfgMongoHost)
      _ <- access pipe master "admin" $ auth cfgMongoUser cfgMongoPassword
      pure pipe


runAsIO :: AppEnv -> IO ()
runAsIO env@Env{..} = run envPort $ runAsApplication env

runAsApplication :: AppEnv -> Application
runAsApplication env = serve (Proxy @API) $ runAsServer env

runAsServer :: AppEnv -> Server API
runAsServer env = hoistServer (Proxy @API) (runAsHandler env) (toServant routes)

runAsHandler :: forall a. AppEnv -> App a -> Handler a
runAsHandler env app = do
  res <- liftIO $ runAsEitherIO env app
  liftEither $ first toHttpError res

runAsEitherIO :: AppEnv -> App a -> IO (Either AppError a)
runAsEitherIO env app = do
  x <- try $ runApp env app
  pure $ mapLeft unAppException  x

-- | convert a business error to an HTTP status code
toHttpError :: AppError -> ServerError
toHttpError (AppError NotFound) = err404
toHttpError (AppError ConcurrentAccess) = err409
toHttpError (AppError NotAnUrl) = err400 { errBody = "The submitted url is not a valid url" }
