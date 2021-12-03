module Core.IdGenerator where

import Core.Urls (ShortUrl (ShortUrl))

genId :: (Monad m) => m ShortUrl
genId = pure $ ShortUrl "foo"
