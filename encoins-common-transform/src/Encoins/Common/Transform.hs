{-# LANGUAGE OverloadedStrings #-}

module Encoins.Common.Transform where

import           Data.Aeson           (encode, ToJSON)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text            (Text, pack)
import           Data.Text.Encoding   (decodeUtf8)


-- Text conversion

toText :: Show a => a -> Text
toText = pack . show


-- JSON conversion

toJsonText :: ToJSON a => a -> Text
toJsonText = decodeUtf8 . toJsonStrict

toJsonStrict :: ToJSON a => a -> ByteString
toJsonStrict = toStrict . encode
