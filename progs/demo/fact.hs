{- This is a simple factorial program which uses the I/O system
   to read the input and print the result -}

module Main where

fact :: Integer -> Integer    
fact 0 = 1
fact (n+1) = (n+1)*fact n
fact _ = error "Negative argument to factorial"

main = appendChan stdout "Type in N: " abort $
       readChan stdin abort $ \ input ->
       appendChan stdout (show (fact (read (head (lines input))))) abort done

