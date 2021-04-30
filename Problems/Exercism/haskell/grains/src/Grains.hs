module Grains (square, total) where

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n
    | n < 1 || n > 64 = Nothing
    | otherwise = Just $ fromIntegral 2^(n-1)

total :: Integer
total = fromJust (square 64) * 2 - 1