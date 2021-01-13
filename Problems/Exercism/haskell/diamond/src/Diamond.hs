module Diamond (diamond) where

import Data.Char ( isAlpha, isUpper )

diamond :: Char -> Maybe [String]
diamond ch
    | isAlpha ch && isUpper ch = Just $ matrix (\c1 c2 -> if c1 == c2 then c1 else ' ') row col
    | otherwise = Nothing
  where row = [ch,pred ch..'A'] ++ ['B'..ch]
        col = ['A'..ch] ++ tail [ch,pred ch..'A']
        matrix f xs ys = [[f x y | x <- xs] | y <- ys]