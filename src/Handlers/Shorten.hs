{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Handlers.Shorten ( shorten) where

import Handlers.Model (RequestUrl (RequestUrl, raw), ShortenedUrl (ShortenedUrl), HostUrl(..))
import qualified Data.Text as T
import Core (WithError, WithTimeProvider, WithUrlRepository, Has, UrlRepository, TimeProvider (getCurrentTimestamp), Url(..), grab, Repository (findById, save), throwError, AppErrorType (ConcurrentAccess), shortenUrl, LongUrl(..), WithLogger, logInfo, logError)

-- | Handler to shorten an URL
shorten :: forall env m. (WithLogger env m, WithError m, WithTimeProvider env m, WithUrlRepository env m, Has HostUrl env) => RequestUrl -> m ShortenedUrl
shorten RequestUrl {..} = go maxTries -- tries `maxTries` times before giving up
  where 
    maxTries = 3
    go :: Int -> m ShortenedUrl
    go x | x < 1 = do
      logError $ "Fail to shorten '" <> raw <> "'"
      throwError ConcurrentAccess
    go n = do
      logInfo $ "Try #" <> counter (n - maxTries) <> ": Shorten url '" <> raw <> "'"
      save' <- save <$> grab @(UrlRepository m)
      findById' <- findById <$> grab @(UrlRepository m)
      hostUrl <- base <$> grab @HostUrl
      timestamp <- grab @(TimeProvider m) >>= getCurrentTimestamp
      let url@Url{..} = shortenUrl timestamp $ LongUrl raw
      maybeAlreadyExists <- findById' urlId
      case maybeAlreadyExists of
        Nothing -> do 
          _ <- save' url
          pure $ ShortenedUrl $ hostUrl <> "/" <> urlId 
        Just _ -> go $ n - 1
    counter = T.pack . show . (+ 1) . abs

