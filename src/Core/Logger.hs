{-# LANGUAGE ConstraintKinds #-}
module Core.Logger (WithLogger, Logger(..), logInfo,logError) where

import qualified Data.Text as T
import Control.Monad.Reader (MonadReader)
import Core.Has (Has, grab)

newtype Logger m = Logger
  { logMsg :: Severity -> T.Text -> m ()
  }

data Severity = Trace | Debug | Info | Warning | Error deriving stock (Eq, Show)

type WithLogger env m = (MonadReader env m, Has (Logger m) env)

base :: forall env m. WithLogger env m => Severity -> T.Text -> m ()
base sev msg = do
  logger <- grab @(Logger m)
  logMsg logger sev msg

logInfo :: forall env m. WithLogger env m => T.Text -> m ()
logInfo = base Info

logError :: forall env m. WithLogger env m => T.Text -> m ()
logError = base Error
