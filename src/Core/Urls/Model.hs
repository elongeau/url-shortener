module Core.Urls.Model where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Represents the long URL with its ID
data Url = Url
  { urlRaw :: T.Text,
    -- | the URL ID used to redirect the the long URL
    urlId :: T.Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Url

newtype LongUrl = LongUrl {lgUrl :: T.Text}
