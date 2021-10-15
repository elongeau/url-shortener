{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Urls.Url where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data Url = Url
  { urlRaw :: T.Text,
    urlId :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)

instance ToJSON Url