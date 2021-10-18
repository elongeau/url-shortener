module Endpoints.Model where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype RequestUrl = RequestUrl
  { raw :: T.Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RequestUrl

instance ToJSON RequestUrl

newtype ShortenedUrl = ShortenedUrl
  { url :: T.Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ShortenedUrl

instance ToJSON ShortenedUrl
