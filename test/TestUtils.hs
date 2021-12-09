module TestUtils where


import Infra (AppEnv,Env(Env, envLogger, envPort, envDB), loadConfig)
import Network.Wai (Application)
import Test.Hspec (SpecWith, Spec, beforeWith)
import qualified Data.Pool as Pool
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Data.Aeson ( ToJSON, encode, encode )
import qualified Data.ByteString as BS
import Test.Hspec.Wai (WaiSession, request, withState)
import Network.Wai.Test (SResponse)
import Test.Hspec.Wai.Matcher (ResponseMatcher)
import Data.String (fromString)
import Data.ByteString.Lazy.UTF8 (toString)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (hContentType)
import Database.MongoDB (access, master, deleteAll)
import Control.Monad (void)
import UrlShortener (mkAppEnv, runAsApplication)
import Core (Logger(Logger))

type TestState = (AppEnv, Application)

withServer :: SpecWith TestState  -> Spec
withServer = withState runServer . beforeWith cleanDB

cleanDB :: TestState -> IO TestState
cleanDB state@(Env {..}, _) = do
  Pool.withResource envDB $ \pipe -> do
    let run act = access pipe master "url-shortener" act
    void $ run $ deleteAll "urls" [([], [])]
  pure state

runServer :: IO (AppEnv, Application)
runServer = do
  env <- loadConfig >>= mkAppEnv >>= withNoLogging
  let app = runAsApplication env
  newEnv <- testWithApplication (pure app) (withPort env)
  pure (newEnv, app)
  where
    withNoLogging :: AppEnv -> IO AppEnv
    withNoLogging env = pure $ env {envLogger = Logger \_ _ -> pure ()}
    withPort :: AppEnv -> Port -> IO AppEnv
    withPort env port = pure $ env {envPort = port}

postJson :: (ToJSON a) => BS.ByteString -> a -> WaiSession AppEnv SResponse
postJson path entity = do
  request methodPost path [(hContentType, "application/json")] (encode entity)

toMatcher :: (ToJSON a) => a -> ResponseMatcher
toMatcher = fromString . toString . encode