-- This program implements Eratosthenes Sieve
-- to generate prime numbers.

module Main where

primes :: [Int]
primes = map head (iterate sieve [2 ..])

sieve :: [Int] -> [Int]
sieve (p:ps) = [x | x <- ps, (x `mod` p) /= 0]

main = appendChan stdout "How many primes? " abort $
       readChan stdin abort $ \ input ->
       appendChan stdout (show (take (read (head (lines input))) primes))
	                 abort done

