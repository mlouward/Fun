module SumOfMultiples (sumOfMultiples) where

import Data.Set (fromList)

multiples :: Integer -> Integer -> [Integer]
multiples n limit
    | n == 0 = [0]
    | otherwise = map (n*) [1..sup]
        where sup = (limit - 1) `div` n

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit
    | null factors = 0
    | otherwise = sum $ fromList $ concat $ map (`multiples` limit) factors