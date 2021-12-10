module UrlShortenerSpec where

import Control.Monad (void)
import Core (Logger (Logger))
import qualified Data.Pool as Pool
import Database.MongoDB (access, deleteAll, master)
import Handlers (RequestUrl (RequestUrl), ShortenedUrl (ShortenedUrl))
import Infra (AppEnv, Env (Env, envDB, envLogger), UrlRoutes (..), loadConfig)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (BaseUrl (baseUrlPort), ClientEnv, ClientM, mkClientEnv, parseBaseUrl, runClientM, ClientError)
import Servant.Client.Generic (AsClientT, genericClient)
import Test.Hspec (Spec, SpecWith, aroundAllWith, beforeAll, beforeAllWith, beforeWith, describe, it, shouldBe)
import UrlShortener (mkAppEnv, runAsApplication)

mainClient :: UrlRoutes (AsClientT ClientM)
mainClient = genericClient

mkEnv :: IO AppEnv
mkEnv = loadConfig >>= mkAppEnv >>= withNoLogging
  where
    withNoLogging :: AppEnv -> IO AppEnv
    withNoLogging env = pure $ env {envLogger = Logger \_ _ -> pure ()}

cleanDB :: AppEnv -> IO AppEnv
cleanDB env@Env {..} = do
  Pool.withResource envDB $ \pipe -> do
    let run act = access pipe master "url-shortener" act
    void $ run $ deleteAll "urls" [([], [])]
  pure env

mkApp :: AppEnv -> IO Application
mkApp = pure . runAsApplication

withClientEnv :: (ClientEnv -> IO ()) -> Application -> IO ()
withClientEnv useClientEnv app = do
  baseUrl <- parseBaseUrl "http://localhost"
  manager <- newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
  Warp.testWithApplication (pure app) $ useClientEnv . clientEnv

withServer :: SpecWith ClientEnv -> Spec
withServer = beforeAll mkEnv . beforeWith cleanDB . beforeAllWith mkApp . aroundAllWith withClientEnv

runTest :: ClientEnv -> (UrlRoutes (AsClientT ClientM) -> ClientM a) ->  IO (Either ClientError a)
runTest cenv func = runClientM (func mainClient) cenv

spec :: Spec
spec = withServer do
  describe "shortening" $ do
    it "shorten any valid url" $ \cenv -> do
      result <- runTest cenv (`postShorten` RequestUrl "http://example.com")
      result `shouldBe` Right (ShortenedUrl "http://localhost:8080/foo")
