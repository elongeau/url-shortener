module ConfigSpec where

import Config (Config (Config), loadConfig)
import Data.ByteString.Char8 (pack)
import System.Environment (setEnv)
import Test.Hspec (Spec, before, describe, it, shouldBe)

spec :: Spec
spec = describe "loading configuration" $ do
  before cleanEnv $
    describe "from environment variables" $ do
      it "should load config when all variables are set" $ do
        setEnv "port" "1234"
        setEnv "db_credentials" "cred"
        cfg <- loadConfig
        cfg `shouldBe` Just (Config 1234 (pack "cred"))
      it "should use default port when it's missing" $ do
        setEnv "db_credentials" "cred"
        cfg <- loadConfig
        cfg `shouldBe` Just (Config 8080 (pack "cred"))
      it "should return nothing when db_credentials are missing" do
        setEnv "port" "1234"
        cfg <- loadConfig
        cfg `shouldBe` Nothing
      it "should return nothing when all are missing" do
        cfg <- loadConfig
        cfg `shouldBe` Nothing
      it "should return nothing when db_credentials are empty" $ do
        setEnv "port" "1234"
        setEnv "db_credentials" ""
        cfg <- loadConfig
        cfg `shouldBe` Nothing

cleanEnv :: IO ()
cleanEnv = do
  setEnv "port" ""
  setEnv "db_credentials" ""
