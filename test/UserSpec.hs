{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UserSpec where

import App
import Control.Monad.Reader (MonadIO (liftIO), MonadTrans (lift), ReaderT (runReaderT), runReader)
import Control.Monad.Writer (MonadWriter (tell), Writer)
import Control.Monad.Writer.Lazy (MonadWriter)
import Domain.Users.User (User (User), email, password, username)
import Endpoints.UserAPI (CreateUser (create))
import Hedgehog (Group (Group), Property, checkParallel, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.TH (discover)
import qualified Hedgehog.Range as Range

tests :: IO Bool
tests =
  checkParallel $$(discover)

hprop_reverse :: Property
hprop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

app :: App User
app =
  let user =
        User
          { username = "John",
            password = "pwd",
            email = "foo@here.com"
          }
   in liftIO $ createUser user

instance CreateUser (Writer [User]) where
  create user = tell [user] >> return user

hinsertUser :: Property
hinsertUser = property $ do
  let env :: AppEnv = _
  result <- runReaderT (unApp app) env
  result === user
