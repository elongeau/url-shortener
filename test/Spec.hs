module Main where

import ApiSpec (runApiSpec)
import Infra (loadConfig)
import Test.Hspec (hspec)
import UrlShortener (mkAppEnv)

main :: IO ()
main = do
  env <- loadConfig >>= mkAppEnv
  hspec $ runApiSpec env
