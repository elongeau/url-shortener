module ServerSpec where

import App (Env (Env))
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 (toString)
import Data.String (IsString (fromString))
import Endpoints.Model (RequestUrl (RequestUrl), ShortenedUrl (ShortenedUrl))
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai.Test (SResponse)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (ResponseMatcher (matchStatus), WaiSession, request, shouldRespondWith, with)
import UrlShortener (runServer)

postJson :: (ToJSON a) => BS.ByteString -> a -> WaiSession st SResponse
postJson path entity = request methodPost path [(hContentType, "application/json")] (encode entity)

spec :: Spec
spec = with (pure $ runServer env) $ do
  describe "POST /shorten" $ do
    it "responds with shortened URL" $ do
      postJson "/shorten" (RequestUrl "http://example.com") `shouldRespondWith` shortenedUrl {matchStatus = 201}
  where
    shortenedUrl :: ResponseMatcher
    shortenedUrl = fromString . toString . encode . ShortenedUrl $ "nope"
    env = Env 8080
