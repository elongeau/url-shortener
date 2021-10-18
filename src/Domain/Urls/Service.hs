module Domain.Urls.Service (Service (..), service) where

import Control.Monad.Reader (MonadReader)
import Domain.Urls.Model (LongUrl (..), Url (..))

newtype Service m = Service
  { shortenUrl :: LongUrl -> m Url
  }

service :: (MonadReader env m) => Service m
service = Service {shortenUrl = shortUrl}

shortUrl :: (MonadReader env m) => LongUrl -> m Url
shortUrl LongUrl {..} =
  pure $
    Url
      { urlRaw = lgUrl,
        urlId = "nope"
      }
