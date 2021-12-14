module Core.Urls.Data where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData (toHeader, toUrlPiece))
import Servant.API (FromHttpApiData (parseUrlPiece))
import qualified Data.Text.Encoding as T

newtype RawUrl = RawUrl {unRaw :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

newtype ShortUrl = ShortUrl {unShort :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

instance FromHttpApiData ShortUrl where
  parseUrlPiece = Right . ShortUrl

instance ToHttpApiData ShortUrl  where
  toHeader (ShortUrl url) = T.encodeUtf8 url
  toUrlPiece (ShortUrl url) = url

-- | Represents the long URL with its ID
data Url = Url
  { urlRaw :: RawUrl,
    -- | the URL ID used to redirect the the long URL
    urlId :: ShortUrl
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

newtype LongUrl = LongUrl {lgUrl :: T.Text}
