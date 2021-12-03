{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Handlers.Redirect ( redirect, UrlForHeader) where

import Servant
    ( addHeader,
      ToHttpApiData(..),
      NoContent(..),
      Header,
      Headers)
import qualified Data.Text.Encoding as T
import Core (WithError,RawUrl(RawUrl), WithUrlRepository, UrlRepository, Url(..), grab, Repository (findById), throwError, AppErrorType (NotFound), WithLogger, logInfo, logError, ShortUrl(ShortUrl))

-- | Allow to add the long URL as Header
newtype UrlForHeader = UrlForHeader Url 
instance ToHttpApiData UrlForHeader  where
  toHeader (UrlForHeader (Url (RawUrl raw) _)) = T.encodeUtf8 raw
  toUrlPiece = undefined -- not used

-- | Handler to redirect to an existing URL
redirect :: forall env m. (WithLogger env m, WithError m, WithUrlRepository env m) => ShortUrl -> m (Headers '[Header "Location" UrlForHeader] NoContent)
redirect shortUrl@(ShortUrl urlId) = do
  repo <- grab @(UrlRepository m)
  maybeUrl <- findById repo shortUrl
  case maybeUrl of
    Nothing -> do
      logError $ "no url for ID: " <> urlId
      throwError NotFound 
    Just url -> do
      logInfo $ "found a url for ID: " <> urlId
      return (addHeader (UrlForHeader url) NoContent)
