module Bob (responseFor) where
import Data.Char
import Data.List

-- ""
-- Question? + CAPS LOCK
-- Question?
-- CAPS LOCK
-- Whatever.

trim = dropWhileEnd isSpace . dropWhile isSpace

responseFor :: String -> String
responseFor text
        | xs == "" = "Fine. Be that way!"
        | question && allCaps = "Calm down, I know what I'm doing!"
        | question = "Sure."
        | allCaps = "Whoa, chill out!"
        | otherwise = "Whatever."
        where xs = trim text
              lettersNum = length [x | x <- xs, isLetter x]
              allCaps = lettersNum == length [x | x <- xs, x `elem` ['A'..'Z']] && lettersNum > 0
              question = last xs == '?'


