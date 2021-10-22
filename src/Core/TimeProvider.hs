module Core.TimeProvider where

-- | provide the current timestamp
newtype TimeProvider m = TimeProvider
  { getCurrentTimestamp :: m Int
  }
