module Domain.Urls.Model where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Url = Url
  { urlRaw :: T.Text,
    urlId :: T.Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Url

newtype LongUrl = LongUrl {lgUrl :: T.Text}
