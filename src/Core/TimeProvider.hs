{-# LANGUAGE ConstraintKinds #-}

module Core.TimeProvider where

import Control.Monad.Reader (MonadReader)
import Core.Has (Has)

-- | provide the current timestamp
newtype TimeProvider m = TimeProvider
  { getCurrentTimestamp :: m Int
  }

-- | Indicate that the function can use a 'TimeProvider'
type WithTimeProvider env m = (MonadReader env m, Has (TimeProvider m) env)
