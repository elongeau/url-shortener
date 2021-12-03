module ApiSpec where

import Control.Monad (void)
import Core (Logger (Logger))
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Pool as Pool
import Data.String (IsString (fromString))
import Database.MongoDB (access, deleteAll, master)
import Handlers (RequestUrl (RequestUrl))
import Handlers.Model (ShortenedUrl (ShortenedUrl))
import Infra (AppEnv, Env (envDB, envLogger, envPort), loadConfig)
import Infra.App (Env (Env))
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Network.Wai.Test (SResponse)
import Test.Hspec (Spec, SpecWith, beforeWith, describe, it)
import Test.Hspec.Wai (ResponseMatcher (matchHeaders, matchStatus), WaiSession, get, request, shouldRespondWith, withState, (<:>))
import UrlShortener (mkAppEnv, runAsApplication)

type TestState = (AppEnv, Application)
withServer :: SpecWith TestState  -> Spec
withServer = withState runServer . beforeWith cleanDB

cleanDB :: TestState -> IO TestState
cleanDB state@(Env {..}, _) = do
  Pool.withResource envDB $ \pipe -> do
    let run act = access pipe master "url-shortener" act
    void $ run $ deleteAll "urls" [([], [])]
  pure state

runServer :: IO (AppEnv, Application)
runServer = do
  env <- loadConfig >>= mkAppEnv >>= withNoLogging
  let app = runAsApplication env
  newEnv <- testWithApplication (pure app) (withPort env)
  pure (newEnv, app)
  where
    withNoLogging :: AppEnv -> IO AppEnv
    withNoLogging env = pure $ env {envLogger = Logger \_ _ -> pure ()}
    withPort :: AppEnv -> Port -> IO AppEnv
    withPort env port = pure $ env {envPort = port}

spec :: Spec
spec = withServer $ do
  describe "Using the API" $ do
    it "creates a short URL and then redirect to original url" $ do
      _ <- postJson "/shorten" (RequestUrl "http://example.com") `shouldRespondWith` (toMatcher (ShortenedUrl "http://localhost:8080/foo")) {matchStatus = 201}
      get "/foo" `shouldRespondWith` 301 {matchHeaders = ["Location" <:> "http://example.com"]}
    it "responds Not-Found when short url is unknown" $ do
      get "/unknown" `shouldRespondWith` 404
    it "fails on existing ID" $ do
      _ <- postJson "/shorten" (RequestUrl "http://example.com")
      postJson "/shorten" (RequestUrl "http://example.com") `shouldRespondWith` 409
    it "refuses invalid URL" $ do
      postJson "/shorten" (RequestUrl "not-an-url") `shouldRespondWith` "The submitted url is not a valid url" {matchStatus = 400}

postJson :: (ToJSON a) => BS.ByteString -> a -> WaiSession AppEnv SResponse
postJson path entity = do
  request methodPost path [(hContentType, "application/json")] (encode entity)

toMatcher :: (ToJSON a) => a -> ResponseMatcher
toMatcher = fromString . toString . encode
