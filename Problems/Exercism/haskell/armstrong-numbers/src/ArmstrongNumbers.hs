module ArmstrongNumbers (armstrong) where

import Data.Char

-- armstrong :: (Int a, Show a) => a -> Bool
armstrong nb = nb == sum [ digitToInt (n !! i) ^ nbLength | i <- [0..nbLength - 1]]
    where n        = show nb
          nbLength = length n