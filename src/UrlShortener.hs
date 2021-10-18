module UrlShortener (main, runServer,runApp,API) where

import App.Env ( Env(..) )
import App.Monad ( App(unApp) )
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import qualified Endpoints.UrlAPI as U
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Proxy (Proxy), hoistServer, ServerT)
import Servant.Server (Server, serve)
import qualified Config as C

type API = U.API

runAsIO :: Env -> App a -> IO a
runAsIO env app = runReaderT (unApp app) env

runAsHandler :: forall a. Env -> App a -> Handler a
runAsHandler env app = liftIO $ runAsIO env app

appServer :: Monad m => ServerT API m
appServer = U.server

server :: Env -> Server API
server env = hoistServer (Proxy @API) (runAsHandler env) appServer

runServer :: Env -> Application
runServer env = serve (Proxy @API) $ server env

runApp :: Env -> IO ()
runApp env@Env{..} = run envPort $ runServer env

setup :: C.Config -> IO Env 
setup C.Config{..} = do 
  let envPort = cfgPort
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
