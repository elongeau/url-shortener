module App.Monad
  ( App (..),
    Env,
    AppEnv,
  )
where

import App.Env (Env)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import UnliftIO (MonadUnliftIO)

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadUnliftIO)

type AppEnv = Env App
