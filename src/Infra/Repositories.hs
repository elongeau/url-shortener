{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Infra.Repositories where

import App (App)
import qualified Data.Text as T
import Database.MongoDB (Action, Document, Field ((:=)), Pipe, Val (val), access, auth, connect, findOne, host, insert, master, select, (!?))
import Domain.Repository (Repository (Repository, findById, save))
import qualified Domain.Urls as Urls
import UnliftIO (MonadIO (liftIO))
import Prelude hiding (id)

urlRepository :: IO (Urls.UrlRepository App)
urlRepository = do
  pipe <- liftIO $ connect (host "localhost")
  _ <- liftIO $ access pipe master "admin" $ auth "root" "example"
  return
    Repository
      { save = save pipe,
        findById = findById pipe
      }
  where
    collection :: T.Text
    collection = "urls"
    run :: Pipe -> Action IO a -> App a
    run pipe act = liftIO $ access pipe master "url-shortener" act
    save :: Pipe -> Urls.Url -> App Urls.Url
    save pipe url@(Urls.Url raw _id) = do
      _ <- run pipe $ insert collection ["url" := val raw, "_id" := val _id]
      pure url
    findById :: Pipe -> T.Text -> App (Maybe Urls.Url)
    findById pipe id = do
      maybeDoc <- run pipe $ findOne $ select ["_id" := val id] collection
      return $ maybeDoc >>= mkUrl
    mkUrl :: Document -> Maybe Urls.Url
    mkUrl doc = Urls.Url <$> (doc !? "url") <*> (doc !? "_id")
