module Endpoints.Model where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
newtype RequestUrl = RequestUrl
  { raw :: T.Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RequestUrl

newtype ShortenedUrl = ShortenedUrl {
    url :: T.Text
}
  deriving stock (Show, Eq, Generic)
  
instance ToJSON ShortenedUrl