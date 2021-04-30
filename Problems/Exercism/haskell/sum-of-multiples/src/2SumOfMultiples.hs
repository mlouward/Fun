module SumOfMultiples (sumOfMultiples) where

import Data.Set (Set, fromList, union)

multiples :: Integer -> Integer -> Set Integer
multiples n limit
    | n == 0 = fromList [0]
    | otherwise = fromList $ map (n*) [1..sup]
        where sup = (limit - 1) `div` n

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit
    | null factors = 0
    | otherwise = sum $ foldr1 (\a b -> union a b) (map (`multiples` limit) factors)