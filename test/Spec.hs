module Main where

import ApiSpec (runApiSpec)
import Control.Exception (bracket)
import Core (Logger (Logger))
import qualified Data.Pool as Pool
import Infra (AppEnv, Env (envDB, envLogger), loadConfig)
import Infra.App (Env (Env))
import Test.Hspec (hspec)
import UrlShortener (mkAppEnv)

main :: IO ()
main =
  bracket
    (loadConfig >>= mkAppEnv)
    (\Env {..} -> Pool.destroyAllResources envDB)
    runTests
  where
    runTests :: AppEnv -> IO ()
    runTests env =
      hspec $
        runApiSpec
          env
            { envLogger = Logger \_ _ -> pure ()
            }
