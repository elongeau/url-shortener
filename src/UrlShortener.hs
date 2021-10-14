module UrlShortener (main) where

import App.Env ( Env(..), WithDB )
import App.Monad ( App(unApp) )
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import qualified Endpoints.UrlAPI as U
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Proxy (Proxy), hoistServer, ServerT)
import Servant.Server (Server, serve)
import qualified Config as C
import DB.Pool ( mkPool )
import DB.Migrations (runDbMigration)

type RealworldAPI = U.API

runAsIO :: Env -> App a -> IO a
runAsIO env app = runReaderT (unApp app) env

runAsHandler :: forall a. Env -> App a -> Handler a
runAsHandler env app = liftIO $ runAsIO env app

appServer ::  WithDB env m => ServerT RealworldAPI m
appServer = U.server

server :: Env -> Server RealworldAPI
server env = hoistServer (Proxy @RealworldAPI) (runAsHandler env) appServer

runServer :: Env -> Application
runServer env = serve (Proxy @RealworldAPI) $ server env

mkAppEnv :: C.Config -> IO Env
mkAppEnv C.Config{..} = do
  let (C.Port envPort) = C.cPort cServer
  envDbPool <- mkPool $ C.dbCredentials cDB
  pure Env{..}

runApp :: Env -> IO ()
runApp env@Env{..} = run envPort $ runServer env

main :: IO ()
main = putStrLn "Starting Realworld app" >> C.loadConfig >>= mkAppEnv >>= \env@Env{..} -> do
    -- _ <- runDbMigration envDbPool 
    runApp env
