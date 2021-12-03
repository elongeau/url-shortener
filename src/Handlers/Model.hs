module Handlers.Model where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | A URL to shorten
newtype RequestUrl = RequestUrl
  { raw :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | The shortened URL
newtype ShortenedUrl = ShortenedUrl
  { url :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | the Host URL
newtype HostUrl = HostUrl
  { hUrl :: T.Text
  }
