module ServerSpec where

import App (Env (..))
import App.Monad (AppEnv)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 (toString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Domain.Repository (Repository (Repository, findById, save))
import Domain.Urls (Url (urlId))
import qualified Domain.Urls as Urls
import Endpoints.Model (RequestUrl (RequestUrl), ShortenedUrl (ShortenedUrl))
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai.Test (SResponse)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (ResponseMatcher (matchStatus), WaiSession, request, shouldRespondWith, with)
import UnliftIO (MonadIO (liftIO))
import UrlShortener (runServer)

postJson :: (ToJSON a) => BS.ByteString -> a -> WaiSession st SResponse
postJson path entity = request methodPost path [(hContentType, "application/json")] (encode entity)

spec :: Spec
spec = with (fmap runServer env) $ do
  describe "POST /shorten" $ do
    it "responds with shortened URL" $ do
      -- TODO assert que l'URL est en DB: utiliser l'autre endpoint
      postJson "/shorten" (RequestUrl "http://example.com") `shouldRespondWith` shortenedUrl {matchStatus = 201}
  where
    shortenedUrl :: ResponseMatcher
    shortenedUrl = fromString . toString . encode . ShortenedUrl $ "nope"

    urlRepository :: (MonadIO m) => IORef (Map T.Text Url) -> Urls.UrlRepository m
    urlRepository ref =
      Repository
        { save = \entity -> liftIO $ do
            db <- liftIO $ readIORef ref
            let newDb = Map.insert (urlId entity) entity db
            liftIO $ writeIORef ref newDb
            pure entity,
          findById = \key -> do
            db <- liftIO $ readIORef ref
            pure $ Map.lookup key db
        }

    ioDb :: IO (IORef (Map T.Text Url))
    ioDb = newIORef Map.empty

    env :: IO AppEnv
    env = do
      db <- liftIO ioDb
      pure
        Env
          { envPort = 8080,
            urlService = Urls.service,
            urlRepository = urlRepository db
          }
