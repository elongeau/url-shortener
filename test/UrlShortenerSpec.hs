module UrlShortenerSpec where

import Core (Logger (Logger))
import Handlers (RequestUrl (RequestUrl), ShortenedUrl (ShortenedUrl))
import Infra (AppEnv, Env (envLogger), UrlRoutes (..), loadConfig)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (BaseUrl (baseUrlPort), ClientEnv, ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)
import Test.Hspec (ActionWith, Spec, SpecWith, describe, it, shouldBe, aroundAll)
import UrlShortener (mkAppEnv, runAsApplication)

app :: IO Application
app = do
  env <- loadConfig >>= mkAppEnv >>= withNoLogging
  pure $ runAsApplication env
  where
    withNoLogging :: AppEnv -> IO AppEnv
    withNoLogging env = pure $ env {envLogger = Logger \_ _ -> pure ()}

mainClient :: UrlRoutes (AsClientT ClientM)
mainClient = genericClient

withApp :: SpecWith ClientEnv -> Spec
withApp = aroundAll withClientEnv

withClientEnv :: ActionWith ClientEnv -> IO ()
withClientEnv useClientEnv = do
  putStrLn "- use client env"
  baseUrl <- parseBaseUrl "http://localhost"
  manager <- newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
  let withPort action = Warp.testWithApplication app action

  withPort $ useClientEnv . clientEnv

spec :: Spec
spec = withApp do
  describe "shortening" $ do
    it "shorten any valid url" $ \cenv -> do
      result <- runClientM (postShorten mainClient (RequestUrl "http://example.com")) cenv
      result `shouldBe` Right (ShortenedUrl "http://localhost:8080/foo")
    it "another test" $ \cenv -> do
      _ <- runClientM (postShorten mainClient (RequestUrl "http://example.com")) cenv
      pure ()
