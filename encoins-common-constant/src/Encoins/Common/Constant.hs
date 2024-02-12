{-# LANGUAGE OverloadedStrings #-}

module Encoins.Common.Constant where

import           Data.String (IsString (..))

-- Text constants

space :: IsString a => a
space = " "

column :: IsString a => a
column = ":"

newLine :: IsString a => a
newLine = "\n"
