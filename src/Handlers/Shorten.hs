{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Handlers.Shorten ( shorten) where

import Handlers.Model (RequestUrl (RequestUrl, raw), ShortenedUrl (ShortenedUrl), BaseUrl(..))
import qualified Data.Text as T
import Core (WithError, WithTimeProvider, WithUrlRepository, Has, UrlRepository, TimeProvider (getCurrentTimestamp), Url(..), grab, Repository (findById, save), throwError, AppErrorType (ConcurrentAccess), shortenUrl, LongUrl(..), WithLogger, logInfo, logError)

shorten :: forall env m. (WithLogger env m, WithError m, WithTimeProvider env m, WithUrlRepository env m, Has BaseUrl env) => RequestUrl -> m ShortenedUrl
shorten RequestUrl {..} = go 3 -- tries 3 times before giving up
  where 
    go :: Int -> m ShortenedUrl
    go 0 = do
      logError $ "Fail to shorten '" <> raw <> "'"
      throwError ConcurrentAccess
    go n = do
      logInfo $ "Try #" <> T.pack (show n) <> ": Shorten url '" <> raw <> "'"
      save' <- save <$> grab @(UrlRepository m)
      findById' <- findById <$> grab @(UrlRepository m)
      baseUrl <- base <$> grab @BaseUrl
      timestamp <- grab @(TimeProvider m) >>= getCurrentTimestamp
      let url@Url{..} = shortenUrl timestamp $ LongUrl raw
      maybeAlreadyExists <- findById' urlId
      case maybeAlreadyExists of
        Nothing -> do 
          _ <- save' url
          pure $ ShortenedUrl $ baseUrl <> "/" <> urlId 
        Just _ -> go $ n - 1
