module ServerSpec where

import App (Env (..))
import App.Monad (AppEnv)
import Core.Repository (Repository (Repository, findById, save))
import Core.TimeProvider (TimeProvider (TimeProvider))
import Core.Urls (Url (urlId))
import qualified Core.Urls as Urls
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 (toString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Endpoints.Model (RequestUrl (RequestUrl), ShortenedUrl (ShortenedUrl))
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai.Test (SResponse)
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Wai (ResponseMatcher (matchHeaders, matchStatus), WaiSession, get, request, shouldRespondWith, with, (<:>))
import UnliftIO (MonadIO (liftIO))
import UrlShortener (runServer)

postJson :: (ToJSON a) => BS.ByteString -> a -> WaiSession st SResponse
postJson path entity = request methodPost path [(hContentType, "application/json")] (encode entity)

spec :: Spec
spec = do
  db <- runIO ioDb
  timeRef <- runIO ioTimeRef
  let env = mkEnv db timeRef
  with (fmap runServer env) $ do
    describe "Using the API" $ do
      it "creates a short URL and then redirect to original url" $ do
        -- _ <- liftIO $ changeTime timeRef 1
        _ <- postJson "/shorten" (RequestUrl "http://example.com") `shouldRespondWith` (toMatcher (ShortenedUrl "1L9zO9O")) {matchStatus = 201}
        get "/1L9zO9O" `shouldRespondWith` 301 {matchHeaders = ["Location" <:> "http://example.com"]}
      it "responds Not Found when short url is unknown" $ do
        get "/unknown" `shouldRespondWith` 404

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
type DB = Map T.Text Url
ioDb :: IO (IORef DB)
ioDb = newIORef Map.empty

ioTimeRef :: IO (IORef Int)
ioTimeRef = newIORef 100000000000

changeTime :: IORef Int -> Int -> IO ()
changeTime ref newTime = do
  writeIORef ref newTime

timeProvider :: MonadIO m => IORef Int -> TimeProvider m
timeProvider ref = TimeProvider $
  liftIO $ readIORef ref

cleanDB :: IORef DB -> IO ()
cleanDB ref = writeIORef ref Map.empty 
  


mkEnv :: IORef DB -> IORef Int -> IO AppEnv
mkEnv db timeRef = do
  pure
    Env
      { envPort = 8080,
        envUrlService = Urls.service,
        envUrlRepository = urlRepository db,
        envTimeProvider = timeProvider timeRef
      }
