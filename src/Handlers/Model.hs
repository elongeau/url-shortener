module Handlers.Model where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | A URL to shorten
newtype RequestUrl = RequestUrl
  { raw :: T.Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RequestUrl

instance ToJSON RequestUrl

-- | The shortened URL
newtype ShortenedUrl = ShortenedUrl
  { url :: T.Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ShortenedUrl

instance ToJSON ShortenedUrl

-- | the Host URL
newtype HostUrl = HostUrl
  { base :: T.Text
  }
