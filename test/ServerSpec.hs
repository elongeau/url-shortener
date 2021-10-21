module ServerSpec where

import App (Env (..))
import App.Monad (AppEnv)
import Data.Aeson (ToJSON , encode)
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
import Test.Hspec.Wai (ResponseMatcher (matchStatus), WaiSession, get, request, shouldRespondWith, with)
import UnliftIO (MonadIO (liftIO))
import UrlShortener (runServer)
import Domain.TimeProvider (TimeProvider(TimeProvider))

postJson :: (ToJSON a) => BS.ByteString -> a -> WaiSession st SResponse
postJson path entity = request methodPost path [(hContentType, "application/json")] (encode entity)

spec :: Spec
spec = with (fmap runServer env) $ do
  describe "POST /shorten" $ do
    it "responds with shortened URL" $ do
      _ <- postJson "/shorten" (RequestUrl "http://example.com") `shouldRespondWith` (toMatcher (ShortenedUrl "1L9zO9O")) { matchStatus = 201}
      get "/1L9zO9O" `shouldRespondWith` toMatcher Urls.Url {urlRaw = "http://example.com", urlId = "1L9zO9O"}
  where
    toMatcher :: (ToJSON a) => a -> ResponseMatcher
    toMatcher = fromString . toString . encode

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
            envUrlService = Urls.service,
            envUrlRepository = urlRepository db,
            envTimeProvider = TimeProvider $ pure 100000000000
          }
