{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Handlers.UrlAPI ( shorten, redirect, UrlForHeader(..)) where

import Handlers.Model (RequestUrl (RequestUrl, raw), ShortenedUrl (ShortenedUrl), BaseUrl(..))
import Servant
    ( addHeader,
      ToHttpApiData(..),
      NoContent(..),
      Header,
      Headers)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Core (WithError, WithTimeProvider, WithUrlRepository, Has, UrlRepository, TimeProvider (getCurrentTimestamp), Url(..), grab, Repository (findById, save), throwError, AppErrorType (NotFound, ConcurrentAccess), shortenUrl, LongUrl(..))

shorten :: forall env m. (WithError m, WithTimeProvider env m, WithUrlRepository env m, Has BaseUrl env) => RequestUrl -> m ShortenedUrl
shorten RequestUrl {..} = go 3 -- tries 3 times before giving up
  where 
    go :: Int -> m ShortenedUrl
    go 0 = throwError ConcurrentAccess
    go n = do
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

newtype UrlForHeader = UrlForHeader Url 
instance ToHttpApiData UrlForHeader  where
  toHeader (UrlForHeader (Url raw _)) = T.encodeUtf8 raw
  toUrlPiece = undefined -- not used

redirect :: forall env m. (WithError m, WithUrlRepository env m) => T.Text -> m (Headers '[Header "Location" UrlForHeader] NoContent)
redirect urlId = do
  repo <- grab @(UrlRepository m)
  maybeUrl <- findById repo urlId
  case maybeUrl of
    Nothing -> throwError NotFound 
    Just url -> return (addHeader (UrlForHeader url) NoContent)
