-- https://wiki.haskell.org/99_questions/1_to_10

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "Only one element"
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Index out of range"
elementAt (_:xs) n 
    | n < 1 = error "Index out of range"
    | otherwise = elementAt xs (n - 1)
    
-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
-- Naive exponential time definition
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- "Optimized" version
myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (last xs == x) && isPalindrome (init xs)

-- Simpler and faster:
isPalindrome' xs = xs == reverse xs

-- Problem 7


-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
-- compress (x:xs)
    -- | x == (head xs) = compress xs
    -- | otherwise = (x:xs)

-- Problem 9


-- Problem 10


-- Problem 11


-- Problem 12


-- Problem 13


-- Problem 14


-- Problem 15


-- Problem 16


-- Problem 17


-- Problem 18


-- Problem 19


-- Problem 20


-- Problem 21


-- Problem 22
range :: Integer -> Integer -> [Integer]
range x y = [x..y]

-- Problem 23


-- Problem 24


-- Problem 25


-- Problem 26
combinations :: Integer -> [a] -> [[a]]

-- Problem 27


-- Problem 28


-- Problem 29


-- Problem 30


-- Problem 31
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = xs == [1]
    where xs = [x | x <- [1..ceiling $ sqrt $ fromIntegral n], n `mod` x == 0]

-- Problem 32
-- Prelude has a `gcd` function too, hence the gcd'
gcd' :: Integer -> Integer -> Integer
gcd' 0 b = b
gcd' a b = gcd (b `mod` a) a
myGCD x y | x < 0     = myGCD (-x) y
          | y < 0     = myGCD x (-y)
          | y < x     = gcd' y x
          | otherwise = gcd' x y

-- Problem 33
coprime :: Integer -> Integer -> Bool
coprime a b = myGCD a b == 1

-- Problem 34
totient :: Integer -> Int
totient 1 = 1
totient m = length [a | a <- [1..m], coprime a m]

-- Problem 35
-- problem: find duplicate when exist.
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
-- primeFactors n = [x | x <- primes, mod n x == 0]
    -- where primes = [x | x <- [1..ceiling $ sqrt $ fromIntegral n], isPrime x]

-- Problem 36


-- Problem 37


-- Problem 38


-- Problem 39


-- Problem 40


-- Problem 41


-- Problem 42


-- Problem 43


-- Problem 44


-- Problem 45


-- Problem 46


-- Problem 47


-- Problem 48


-- Problem 49


-- Problem 50


-- Problem 51


-- Problem 52


-- Problem 53


-- Problem 54


-- Problem 55


-- Problem 56


-- Problem 57


-- Problem 58


-- Problem 59


-- Problem 60


-- Problem 61


-- Problem 62


-- Problem 63


-- Problem 64


-- Problem 65


-- Problem 66


-- Problem 67


-- Problem 68


-- Problem 69


-- Problem 70


-- Problem 71


-- Problem 72


-- Problem 73


-- Problem 74


-- Problem 75


-- Problem 76


-- Problem 77


-- Problem 78


-- Problem 79


-- Problem 80


-- Problem 81


-- Problem 82


-- Problem 83


-- Problem 84


-- Problem 85


-- Problem 86


-- Problem 87


-- Problem 88


-- Problem 89


-- Problem 90


-- Problem 91


-- Problem 92


-- Problem 93


-- Problem 94


-- Problem 95


-- Problem 96


-- Problem 97


-- Problem 98


-- Problem 99


