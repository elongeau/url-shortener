module UrlShortener (main, runServer,runApp,API) where

import App.Env ( Env(..), Has )
import App.Monad ( App(unApp), AppEnv )
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT), MonadReader)
import qualified Endpoints.UrlAPI as U
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Proxy (Proxy), hoistServer, ServerT)
import Servant.Server (Server, serve)
import qualified Config as C
import qualified Domain.Urls.Service as Urls

type API = U.API

runAsIO :: AppEnv -> App a -> IO a
runAsIO env app = runReaderT (unApp app) env

runAsHandler :: forall a. AppEnv -> App a -> Handler a
runAsHandler env app = liftIO $ runAsIO env app

appServer :: forall env m . (MonadReader env m, Has (Urls.Service m) env) => ServerT API m
appServer = U.server

server :: AppEnv -> Server API
server env = hoistServer (Proxy @API) (runAsHandler env) appServer

runServer :: AppEnv -> Application
runServer env = serve (Proxy @API) $ server env

runApp :: AppEnv -> IO ()
runApp env@Env{..} = run envPort $ runServer env

setup :: C.Config -> IO AppEnv 
setup C.Config{..} = do 
  let envPort = cfgPort
  let urlService = Urls.service
  pure Env{..}

main :: IO ()
main = do
  maybeConf <- C.loadConfig
  case maybeConf of
    Nothing -> putStrLn "Can't connect to DB"
    Just conf -> do
      env <- setup conf
      putStrLn "Starting Realworld app"
      runApp env
