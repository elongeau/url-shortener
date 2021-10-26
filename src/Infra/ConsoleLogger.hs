module Infra.ConsoleLogger where

import UnliftIO (MonadIO (liftIO))
import Core (Logger(..))
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

consoleLogger :: (MonadIO m) => IO (Logger m)
consoleLogger = pure $ Logger $ \sev msg -> do
  currentTime <- liftIO getCurrentTime
  liftIO $ putStrLn $ "[" <> iso8601Show currentTime <> "] [" <> show sev <> "] " <> show msg
  pure ()
