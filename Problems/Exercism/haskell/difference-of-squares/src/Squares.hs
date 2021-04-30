module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = nb^2
    where nb = sum [1..n]

sumOfSquares :: Integral a => a -> a
sumOfSquares n = foldr (\x y -> x*x + y) 0 [1..n]
