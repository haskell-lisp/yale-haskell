-- Quick sort for Haskell.

module Main where

qs :: [Int] -> [Int]
qs []     = []
qs (a:as) = qs [x | x <- as, x <= a] ++ [a] ++ qs [x | x <- as, x > a]

main =
  appendChan stdout "Enter a list of integers separated by \",\"\n" abort $
  readChan stdin abort $ \ input ->
  appendChan stdout (show (qs (read ("[" ++ (head (lines input)) ++ "]"))))
             abort done
