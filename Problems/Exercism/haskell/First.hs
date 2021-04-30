-- Comments
{- Multiline
comments -}
import Data.List
import System.IO

-- Int
-- maxInt = maxBound :: Int
-- minInt = minBound :: Int

-- Integer: unbound
-- Float: Single-Precision
-- Double: Double-Precision
-- Bool
-- Char: 'e'
-- Tuple (pairs with 2 values)

-- Impose a type to a constant
-- a :: Int
-- a = 12

-- sumOfNums a b = sum [a..b]

-- Example of :t sqrt
-- sqrt :: Floating a => a -> a
-- So we have to convert our Int to float:
-- num = 9 :: Int
-- sqrtOf9 = sqrt (fromIntegral num)

test :: Int -> Int
test x = x - 1

factorial 0 = 1
factorial n = n * factorial (n - 1) 

isOdd = odd

isEven = even






















