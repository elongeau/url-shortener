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
      Headers, FromHttpApiData)
import qualified Data.Text.Encoding as T
import Core (WithError,RawUrl(RawUrl), WithUrlRepository, UrlRepository, Url(..), grab, Repository (findById), throwError, AppErrorType (NotFound), WithLogger, logInfo, logError, ShortUrl(ShortUrl))
import Servant.API (FromHttpApiData(parseUrlPiece))
import qualified Data.Text as T

-- | Allow to add the long URL as Header
newtype UrlForHeader = UrlForHeader T.Text 
instance ToHttpApiData UrlForHeader  where
  toHeader (UrlForHeader raw) = T.encodeUtf8 raw
  toUrlPiece = undefined -- not used

instance FromHttpApiData UrlForHeader where
  parseUrlPiece = Right . UrlForHeader

-- | Handler to redirect to an existing URL
redirect :: forall env m. (WithLogger env m, WithError m, WithUrlRepository env m) => ShortUrl -> m (Headers '[Header "Location" UrlForHeader] NoContent)
redirect shortUrl@(ShortUrl urlId) = do
  repo <- grab @(UrlRepository m)
  maybeUrl <- findById repo shortUrl
  case maybeUrl of
    Nothing -> do
      logError $ "no url for ID: " <> urlId
      throwError NotFound 
    Just (Url (RawUrl raw) _ ) -> do
      logInfo $ "found a url for ID: " <> urlId
      return (addHeader (UrlForHeader raw) NoContent)
