   
-- This is a parallel varient of factorial

module Main where

fac :: Int -> Int
fac 0 = 1
fac n = pfac 1 n

pfac :: Int -> Int -> Int
pfac low high | low == high     = low
              | low + 1 == high = (low * high)
              | otherwise       = pfac low mid * pfac (mid + 1) high
    where
       mid = (high + low) `div` 2

main = appendChan stdout "Type in N: " abort $
       readChan stdin abort $ \ input ->
       appendChan stdout (show (fac (read (head (lines input))))) abort done


