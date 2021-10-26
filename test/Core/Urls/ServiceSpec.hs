module Core.Urls.ServiceSpec where

import Core (LongUrl (..), Url (urlId, urlRaw), shortenUrl)
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

spec :: Spec
spec = describe "encoding an URL" $ do
  it "generate a 7 characters long id" $
    hedgehog $ do
      (x :: Int) <- forAll $ Gen.integral (Range.exponential 0 200000000000)
      let f = T.length . urlId . flip shortenUrl (LongUrl "")
      f x === 7
  it "return the original URL" $
    hedgehog $ do
      url <- forAll $ Gen.text (Range.linear 20 50) Gen.alpha
      let result = urlRaw $ shortenUrl 0 (LongUrl url)
      result === url
