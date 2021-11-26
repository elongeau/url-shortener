{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Handlers.Shorten ( shorten) where

import Handlers.Model (RequestUrl (RequestUrl, raw), ShortenedUrl (ShortenedUrl), HostUrl(..))
import qualified Data.Text as T
import Core (WithError, WithUrlRepository, Has, UrlRepository, Url(..), grab, Repository (findById, save), throwError, AppErrorType (ConcurrentAccess, NotAnUrl), WithLogger, logInfo, logError, genId)

-- | Handler to shorten an URL
shorten :: forall env m. (WithLogger env m, WithError m,  WithUrlRepository env m, Has HostUrl env) => RequestUrl -> m ShortenedUrl
shorten req | isInvalid req = do
  logError "The submitted url is invalid"
  throwError NotAnUrl
shorten RequestUrl{..} = go maxTries -- tries `maxTries` times before giving up
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
      hostUrl <- hUrl <$> grab @HostUrl
      urlId <- genId 
      let urlRaw = raw
      maybeAlreadyExists <- findById' urlId
      case maybeAlreadyExists of
        Nothing -> do 
          _ <- save' $ Url {..}
          pure $ ShortenedUrl $ hostUrl <> "/" <> urlId 
        Just _ -> go $ n - 1
    counter = T.pack . show . (+ 1) . abs

isInvalid :: RequestUrl -> Bool 
isInvalid RequestUrl{..} = not $ T.isPrefixOf "http" raw