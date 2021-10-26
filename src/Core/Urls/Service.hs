{-# LANGUAGE ConstraintKinds #-}

module Core.Urls.Service (shortenUrl) where

import Core.Urls.Model (LongUrl (..), Url (..))
import qualified Data.Text as T

-- | generate the URL ID
shortenUrl :: Int -> LongUrl -> Url
shortenUrl timestamp LongUrl {..} =
  let urlId = toBase62 (timestamp + 100000000000)
   in Url
        { urlRaw = lgUrl,
          urlId = urlId
        }

toBase62 :: Int -> T.Text
toBase62 x =
  if x == 0
    then ""
    else
      let v = x `mod` 62
          c = T.index characters v
       in T.snoc (toBase62 (x `div` 62)) c
  where
    characters :: T.Text
    characters = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
