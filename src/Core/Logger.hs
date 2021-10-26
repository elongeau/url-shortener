{-# LANGUAGE ConstraintKinds #-}
module Core.Logger (WithLogger, Logger(..), logInfo,logError) where

import qualified Data.Text as T
import Control.Monad.Reader (MonadReader)
import Core.Has (Has, grab)

newtype Logger m = Logger
  { logMsg :: Severity -> T.Text -> m ()
  }

data Severity = Info | Error deriving stock (Eq, Show)

-- | Indicate that the function can use a 'Logger'
type WithLogger env m = (MonadReader env m, Has (Logger m) env)

base :: forall env m. WithLogger env m => Severity -> T.Text -> m ()
base sev msg = do
  logger <- grab @(Logger m)
  logMsg logger sev msg

-- | Helper to log at Info level
logInfo :: forall env m. WithLogger env m => T.Text -> m ()
logInfo = base Info

-- | Helper to log at Error level
logError :: forall env m. WithLogger env m => T.Text -> m ()
logError = base Error
