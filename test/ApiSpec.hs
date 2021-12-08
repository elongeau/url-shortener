module ApiSpec where

import Handlers (RequestUrl (RequestUrl))
import Handlers.Data (ShortenedUrl (ShortenedUrl))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (ResponseMatcher (matchHeaders, matchStatus), get, shouldRespondWith, (<:>))
import TestUtils (postJson, toMatcher, withServer)

-- | These tests are lilke golden-test for the API: they assert that the API respond to expect HTTP methods
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
