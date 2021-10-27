module ApiSpec where

import Core (Logger (..), Repository (Repository, findById, save), TimeProvider (..), Url (Url, urlId), UrlRepository)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 (toString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Handlers (HostUrl (HostUrl), RequestUrl (RequestUrl), ShortenedUrl (ShortenedUrl))
import Infra (AppEnv, Env (..))
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai.Test (SResponse)
import Test.Hspec (Spec, before, describe, it, runIO)
import Test.Hspec.Wai (ResponseMatcher (matchHeaders, matchStatus), WaiSession, get, request, shouldRespondWith, with, (<:>))
import UnliftIO (MonadIO (liftIO))
import UrlShortener (runAsApplication)

spec :: Spec
spec = do
  db <- runIO ioDb
  timeRef <- runIO ioTimeRef
  let env = mkEnv db timeRef
  before (cleanDB db) . with (pure $ runAsApplication env) $ do
    describe "Using the API" $ do
      it "creates a short URL and then redirect to original url" $ do
        _ <- postJson "/shorten" (RequestUrl "http://example.com") `shouldRespondWith` (toMatcher (ShortenedUrl "http://localhost:8080/3wj9CjC")) {matchStatus = 201}
        get "/3wj9CjC" `shouldRespondWith` 301 {matchHeaders = ["Location" <:> "http://example.com"]}
      it "responds Not-Found when short url is unknown" $ do
        get "/unknown" `shouldRespondWith` 404
      it "fails on existing ID" $ do
        _ <- postJson "/shorten" (RequestUrl "http://example.com")
        postJson "/shorten" (RequestUrl "http://example.com") `shouldRespondWith` 409
      it "refuses invalid URL" $ do
        postJson "/shorten" (RequestUrl "not-an-url") `shouldRespondWith` "The submitted url is not a valid url" { matchStatus = 400 }

postJson :: (ToJSON a) => BS.ByteString -> a -> WaiSession st SResponse
postJson path entity = request methodPost path [(hContentType, "application/json")] (encode entity)

toMatcher :: (ToJSON a) => a -> ResponseMatcher
toMatcher = fromString . toString . encode

type DB = Map T.Text Url

urlRepository :: (MonadIO m) => IORef DB -> UrlRepository m
urlRepository ref =
  Repository
    { save = \entity@Url {..} -> liftIO $ do
        db <- liftIO $ readIORef ref
        let newDb = Map.insert urlId entity db
        liftIO $ writeIORef ref newDb
        pure entity,
      findById = \key -> do
        db <- liftIO $ readIORef ref
        pure $ Map.lookup key db
    }

ioDb :: IO (IORef DB)
ioDb = newIORef Map.empty

ioTimeRef :: IO (IORef Int)
ioTimeRef = newIORef 100000000000

timeProvider :: MonadIO m => IORef Int -> TimeProvider m
timeProvider ref =
  TimeProvider $
    liftIO $ readIORef ref

cleanDB :: IORef DB -> IO ()
cleanDB ref = writeIORef ref Map.empty

mkEnv :: IORef DB -> IORef Int -> AppEnv
mkEnv db timeRef =
  Env
    { envPort = 8080,
      envUrlRepository = urlRepository db,
      envTimeProvider = timeProvider timeRef,
      envLogger = noLogging,
      envHostUrl = HostUrl "http://localhost:8080"
    }
  where
    noLogging = Logger $ \_ _ -> pure ()
