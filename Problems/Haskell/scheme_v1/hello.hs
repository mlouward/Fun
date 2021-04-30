module Main where

import System.Environment

main :: IO ()
-- 1.
-- main = do
--   args <- getArgs
--   putStrLn ("Hello, " ++ args !! 0 ++ " and " ++ args !! 1)
-- 2.
-- main = do
--   args <- getArgs
--   print $ read (head args) + read (args !! 1)
-- 3.
main = do
  putStrLn "Enter some text: "
  args <- getLine
  putStrLn args