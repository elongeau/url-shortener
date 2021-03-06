{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Infra.MongoRepository where

import Core (RawUrl (RawUrl), Repository (Repository, findById, save), ShortUrl (ShortUrl), Url (Url), UrlRepository)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import Database.MongoDB (Action, Document, Field ((:=)), Pipe, Val (val), access, findOne, insert, master, select, (!?))
import Infra.App (App, DbPool)
import UnliftIO (MonadIO (liftIO))
import Prelude hiding (id)

mkUrlRepository :: DbPool -> UrlRepository App
mkUrlRepository pool =
  Repository
    { save = save,
      findById = findById
    }
  where
    collection :: T.Text
    collection = "urls"
    run :: Action IO a -> App a
    run act = withPool pool \pipe -> access pipe master "url-shortener" act
    save :: Url -> App Url
    save url@(Url (RawUrl raw) (ShortUrl _id)) = do
      _ <- run $ insert collection ["url" := val raw, "_id" := val _id]
      pure url
    findById :: ShortUrl -> App (Maybe Url)
    findById (ShortUrl id) = do
      maybeDoc <- run $ findOne $ select ["_id" := val id] collection
      return $ maybeDoc >>= mkUrl
    mkUrl :: Document -> Maybe Url
    mkUrl doc = Url <$> (RawUrl <$> (doc !? "url")) <*> (ShortUrl <$> (doc !? "_id"))

withPool :: DbPool -> (Pipe -> IO b) -> App b
withPool pool f = liftIO $ Pool.withResource pool f
