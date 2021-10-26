module Infra.ConfigSpec where

import Infra (Config (Config), loadConfig)
import System.Environment (setEnv)
import Test.Hspec (Spec, before, describe, it, shouldBe)

spec :: Spec
spec = describe "loading configuration" $ do
  before cleanEnv $
    describe "from environment variables" $ do
      it "should load config when all variables are set" $ do
        setEnv "SERVER_PORT" "1234"
        setEnv "MONGO_HOST" "127.0.0.1"
        setEnv "MONGO_USER" "user"
        setEnv "MONGO_PASSWORD" "pwd"
        setEnv "BASE_URL" "https://shorten.li/"
        cfg <- loadConfig
        cfg `shouldBe` Config 1234 "127.0.0.1" "user" "pwd" "https://shorten.li/"
      it "should use default values" $ do
        cfg <- loadConfig
        cfg `shouldBe` Config 8080 "localhost" "root" "password" "http://localhost:8080"

cleanEnv :: IO ()
cleanEnv = do
  setEnv "SERVER_PORT" ""
  setEnv "MONGO_HOST" ""
  setEnv "MONGO_USER" ""
  setEnv "MONGO_PASSWORD" ""
  setEnv "BASE_URL" ""
