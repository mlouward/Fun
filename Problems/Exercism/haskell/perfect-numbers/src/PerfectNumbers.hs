module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n < 1 = Nothing
    | otherwise = if aliq == n then Just Perfect else if aliq > n then Just Abundant else Just Deficient
        where aliq = sum [x | x <- [1..(n `div` 2)], n `rem` x == 0]
