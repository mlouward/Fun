-- HackerRank problems

-- Reverse without reverse function
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev (xs) ++ (x:[])

-- Sum of odd elements
sumOfOdds :: [Integer] -> Integer
sumOfOdds arr = sum [x | x <- arr, x `mod` 2 == 1] 

-- List Length without 'length'
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- Absolute value of every element in a list
absList :: [Int] -> [Int]
absList arr = [abs (x) | x <- arr]

-- exponential via series expansion
exp' :: Double -> Double
exp' x = sum [(x**n)/(product [1..n]) | n <- [0..9]]

