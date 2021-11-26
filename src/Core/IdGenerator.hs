module Core.IdGenerator where

import qualified Data.Text as T

genId :: (Monad m) => m T.Text
genId = pure "foo"
