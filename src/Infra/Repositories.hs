{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Infra.Repositories where

import App (App)
import Core.Repository (Repository (Repository, findById, save))
import qualified Core.Urls as Urls
import qualified Data.Text as T
import Database.MongoDB (Action, Document, Field ((:=)), Pipe, Val (val), access, findOne, insert, master, select, (!?))
import UnliftIO (MonadIO (liftIO))
import Prelude hiding (id)

mkUrlRepository :: Pipe -> Urls.UrlRepository App
mkUrlRepository pipe =
  Repository
    { save = save,
      findById = findById
    }
  where
    collection :: T.Text
    collection = "urls"
    run :: Action IO a -> App a
    run act = liftIO $ access pipe master "url-shortener" act
    save :: Urls.Url -> App Urls.Url
    save url@(Urls.Url raw _id) = do
      _ <- run $ insert collection ["url" := val raw, "_id" := val _id]
      pure url
    findById :: T.Text -> App (Maybe Urls.Url)
    findById id = do
      maybeDoc <- run $ findOne $ select ["_id" := val id] collection
      return $ maybeDoc >>= mkUrl
    mkUrl :: Document -> Maybe Urls.Url
    mkUrl doc = Urls.Url <$> (doc !? "url") <*> (doc !? "_id")
