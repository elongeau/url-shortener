{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}

module Infra.App.Monad
  ( App (..),
    Env,
    AppEnv,
    runApp,
  )
where

import Control.Exception (catch, throwIO)
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.Except (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT, runReaderT))
import Core (AppError, AppException (..))
import Infra.App.Env (Env)
import UnliftIO (MonadUnliftIO)

-- | The main Monad
newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadUnliftIO)

type AppEnv = Env App

-- | To allow to throw and catch an error
instance MonadError AppError App where
  throwError :: AppError -> App a
  throwError = liftIO . throwIO . AppException
  {-# INLINE throwError #-}

  catchError :: forall a. App a -> (AppError -> App a) -> App a
  catchError action handler = App $
    ReaderT $ \env -> do
      let ioAction = runApp env action
      ioAction `catch` \(AppException e) -> runApp env $ handler e
  {-# INLINE catchError #-}

runApp :: AppEnv -> App a -> IO a
runApp env app = runReaderT (unApp app) env
