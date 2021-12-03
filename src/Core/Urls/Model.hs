module Core.Urls.Model where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (FromHttpApiData)
import Servant.API (FromHttpApiData (parseUrlPiece))

newtype RawUrl = RawUrl {unRaw :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

newtype ShortUrl = ShortUrl {unShort :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

instance FromHttpApiData ShortUrl where
  parseUrlPiece = Right . ShortUrl

-- | Represents the long URL with its ID
data Url = Url
  { urlRaw :: RawUrl,
    -- | the URL ID used to redirect the the long URL
    urlId :: ShortUrl
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

newtype LongUrl = LongUrl {lgUrl :: T.Text}
