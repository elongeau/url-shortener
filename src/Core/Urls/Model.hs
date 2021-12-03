module Core.Urls.Model where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype RawUrl = RawUrl {unRaw :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON)

-- | Represents the long URL with its ID
data Url = Url
  { urlRaw :: RawUrl,
    -- | the URL ID used to redirect the the long URL
    urlId :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

newtype LongUrl = LongUrl {lgUrl :: T.Text}
