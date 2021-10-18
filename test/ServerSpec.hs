module ServerSpec where

import App (Env (Env))
import Data.Aeson (encode)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.String (IsString (fromString))
import Endpoints.Model (RequestUrl (RequestUrl), ShortenedUrl (ShortenedUrl))
import Network.HTTP.Types (hContentType, methodPost)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Test (SResponse)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (ResponseMatcher (matchStatus), WaiSession, request, shouldRespondWith, with)
import UrlShortener (runServer)

withApp :: Env -> (Warp.Port -> IO ()) -> IO ()
withApp env =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure $ runServer env)

postJson :: WaiSession st SResponse
postJson = request methodPost "/shorten" [(hContentType, "application/json")] (encode $ RequestUrl "")

spec :: Spec
spec = with (pure $ runServer env) $ do
  describe "POST /shorten" $ do
    it "responds with shortened URL" $ do
      let response = postJson
      response `shouldRespondWith` shortenedUrl {matchStatus = 201}
  where
    shortenedUrl :: ResponseMatcher
    shortenedUrl = fromString . toString . encode . ShortenedUrl $ "nope"
    env = Env 8080
