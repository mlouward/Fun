module DNA (toRNA') where

-- toRNA :: String -> Either Char String
-- toRNA [] = []
-- toRNA (x:xs) | x == 'G'  = Right ('C':toRNA xs)
             -- | x == 'C'  = Right ('G':toRNA xs)
             -- | x == 'T'  = Right ('A':toRNA xs)
             -- | x == 'A'  = Right ('U':toRNA xs)
             -- | otherwise = Left x

toRNA' :: String -> Either Char String
toRNA' xs = mapM complement xs
    where complement 'G' = Right 'C'
          complement 'C' = Right 'G'
          complement 'T' = Right 'A'
          complement 'A' = Right 'U'
          complement other = Left other