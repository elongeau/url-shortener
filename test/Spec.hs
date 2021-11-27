module Main where

import ApiSpec (runApiSpec)
import Core (Logger (Logger))
import Infra (Env (envLogger), loadConfig)
import Test.Hspec (hspec)
import UrlShortener (mkAppEnv)

main :: IO ()
main = do
  env <- loadConfig >>= mkAppEnv
  hspec $
    runApiSpec
      env
        { envLogger = Logger \_ _ -> pure ()
        }
