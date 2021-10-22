{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Core.Error where

import Control.Exception.Base (Exception)
import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as E

-- | Exception wrapper around 'AppError'. Useful when you need to throw/catch
-- 'AppError' as 'Exception'.
newtype AppException = AppException
  { unAppException :: AppError
  }
  deriving stock (Show)
  deriving anyclass (Exception)

newtype AppError = AppError
  { appErrorType :: AppErrorType
  }
  deriving stock (Show, Eq)

data AppErrorType = NotFound deriving stock (Show, Eq)

type WithError m = MonadError AppError m

-- | Specialized version of 'E.throwError'.
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError
{-# INLINE throwError #-}
