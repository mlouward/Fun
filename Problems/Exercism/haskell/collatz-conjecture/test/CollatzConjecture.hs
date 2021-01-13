module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
	| n <= 0 = Nothing
	| otherwise = Just $ fromIntegral $ length $ col n
		where col 1 = []
			  col n
				  | even n =  n : col (n `div` 2)
				  | odd n  =  n : col (n * 3 + 1)
