{-# LANGUAGE OverloadedStrings #-}
module Acronym (abbreviate) where

import qualified Data.Text as T
import           Data.Text (Text, split, head, pack, unpack, toUpper)

abbreviate :: String -> String
abbreviate xs = [T.head $ toUpper x | x <- (split (\x -> x == '-' || x ==' ') (pack xs)), not $ T.null x]