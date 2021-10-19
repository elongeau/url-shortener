module Domain.Has where

import Control.Monad.Reader (MonadReader, asks)
class Has field env where
  obtain :: env -> field

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field