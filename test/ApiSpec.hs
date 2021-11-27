module ApiSpec where

import Control.Monad (void)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 (toString)
import Data.String (IsString (fromString))
import Database.MongoDB (Action, Pipe, access, deleteAll, master)
import Handlers (RequestUrl (RequestUrl), ShortenedUrl (ShortenedUrl))
import Infra (AppEnv, Env (envDB))
import Infra.App (Env (Env))
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai.Test (SResponse)
import Test.Hspec (Spec, before_, describe, it)
import Test.Hspec.Wai (ResponseMatcher (matchHeaders, matchStatus), WaiSession, get, request, shouldRespondWith, with, (<:>))
import UrlShortener (runAsApplication)

runApiSpec :: AppEnv -> Spec
runApiSpec env@Env {..} = do
  before_ (cleandDB envDB) . with (pure $ runAsApplication env) $ do
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

cleandDB :: Pipe -> IO ()
cleandDB pipe = void $ run $ deleteAll "urls" [([], [])]
  where
    run :: Action IO a -> IO a
    run act = access pipe master "url-shortener" act

postJson :: (ToJSON a) => BS.ByteString -> a -> WaiSession st SResponse
postJson path entity = request methodPost path [(hContentType, "application/json")] (encode entity)

toMatcher :: (ToJSON a) => a -> ResponseMatcher
toMatcher = fromString . toString . encode
