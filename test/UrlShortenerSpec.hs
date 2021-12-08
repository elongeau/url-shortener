module UrlShortenerSpec where

import Core (Logger (Logger))
import Infra (AppEnv, Env (envLogger), UrlRoutes(..), loadConfig)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (BaseUrl (baseUrlPort), ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)
import Test.Hspec (Spec, around, runIO, describe, it, shouldBe, SpecWith)
import UrlShortener (mkAppEnv, runAsApplication)
import Handlers (RequestUrl(RequestUrl), ShortenedUrl (ShortenedUrl))

userApp :: IO Application
userApp = do
  env <- loadConfig >>= mkAppEnv >>= withNoLogging
  pure $ runAsApplication env
  where
    withNoLogging :: AppEnv -> IO AppEnv
    withNoLogging env = pure $ env {envLogger = Logger \_ _ -> pure ()}


mainClient :: UrlRoutes (AsClientT ClientM)
mainClient = genericClient

withApp :: SpecWith Warp.Port -> Spec
withApp = around (Warp.testWithApplication userApp)

spec :: Spec
spec = withApp do
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

  describe "shortening" $ do
    it "shorten any valid url" $ \port -> do
      result <- runClientM (postShorten mainClient (RequestUrl "http://example.com")) (clientEnv port)
      result `shouldBe` Right (ShortenedUrl "http://localhost:8080/foo")
