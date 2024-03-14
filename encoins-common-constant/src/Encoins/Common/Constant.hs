{-# LANGUAGE OverloadedStrings #-}

module Encoins.Common.Constant where

import           Data.String (IsString (..))

-- Text constants

space :: IsString a => a
space = " "

column :: IsString a => a
column = ":"

dash :: IsString a => a
dash = "-"

newLine :: IsString a => a
newLine = "\n"
