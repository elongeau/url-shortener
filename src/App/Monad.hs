module App.Monad
  ( App (..),
    Env,
  )
where

import App.Env (Env)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import UnliftIO (MonadUnliftIO)

newtype App a = App
  { unApp :: ReaderT Env IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)