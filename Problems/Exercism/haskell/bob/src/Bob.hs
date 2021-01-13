module Bob (responseFor) where
import Data.Char
import Data.List

trim = dropWhileEnd isSpace . dropWhile isSpace

responseFor :: String -> String
responseFor text
        | null xs = "Fine. Be that way!"
        | question && allCaps = "Calm down, I know what I'm doing!"
        | question = "Sure."
        | allCaps = "Whoa, chill out!"
        | otherwise = "Whatever."
        where xs = trim text
              lettersNum = length $ filter isLetter xs
              allCaps = lettersNum == (length $ filter isUpper xs) && lettersNum > 0
              question = last xs == '?'