module Core.Has where

import Control.Monad.Reader (MonadReader, asks)

-- | Allow to access a peculiar field in 'env'
class Has field env where
  obtain :: env -> field

-- | Helper to grab a field inside 'env'
grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field