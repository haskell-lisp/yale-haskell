{-*********************************************************************
    MODULE R_UTILITY
  
      This module contains all the basic utility functions that the other
    modules need to have to write their code. These are made generally
    low level functions, manipulating vectors or defining very 
    general functions  

**********************************************************************-}


module R_Utility (vtovf,vftov,
		  vplus, vmin, mid, partway,
                  mag,
                  reduce, power, i,
                  member, repeat, zip1, zip2, zip3, rept, replicate,
                  mapc, 
                  append, flatten, rptseq, osc
                  ) where

import R_Ptypes


-- CONVERSION

  -- vtovf takes a vector of integers, and converts it to a vector of floats
vtovf :: Vec -> Vecfloat
vtovf (x,y) = (fromIntegral x,fromIntegral y)

  -- vftov takes a vector of floats and converts it to a vector of integers.
  -- It rounds the floats off to do this.
vftov :: Vecfloat -> Vec
vftov (x,y) = (round x,round y)


-- VECTOR OPERATIONS:

  -- vector addition
vplus:: Vec -> Vec -> Vec
vplus (a,b) (c,d) = (a+c,b+d)

  -- vector substraction
vmin:: Vec -> Vec -> Vec
vmin (a,b) (c,d) = (a-c,b-d)

  -- finds the midpoint between two vectors
mid:: Vec -> Vec -> Vec
mid (x1,y1) (x2,y2) = (div (x1+x2) 2,div (y1+y2) 2 )

  -- finds a point p/q along the way between two vectors
partway :: Int -> Int -> [Vec] -> Vec
partway p q [(x1,y1),(x2,y2)]
        = vplus (x1,y1) ( div (p*(x2-x1)) q, div (p*(y2-y1)) q )

  -- finds the magnitude of two vectors
mag :: Vec -> Int
mag p = round (magfloat (vtovf p))

magfloat :: Vecfloat -> Float
magfloat (x,y) = sqrt (x*x + y*y)

  -- returns a vector at right angles to the input vector
normal:: Vec -> Vec
normal (x,y) = (-y,x)

  -- returns the first vector projected onto the second
project:: Vec -> Vec -> Vec
project (vx,vy) (wx,wy) = partway (vx*wx+vy*wy) (mw*mw) [(0,0),(wx,wy)]
			     where mw = mag (wx,wy)


-- HIGHER-ORDER FUNCTIONS:

  -- just foldr1. It applies a function of two inputs to an entire list 
  -- recursively, and displays the single element result
reduce :: (a->a->a) -> [a] -> a
reduce = foldr1

  -- power applies a single function n times to a seed
power :: Int -> (a->a) -> a -> a
power 0 f seed = seed
power (n+1) f seed = f (power n f seed)

  -- i takes an element and returns an infinite list of them
i :: a -> [a]
i x = x: (i x)

  -- checks to see if x is in the list of xs
member :: (Eq a) => [a] -> a -> Bool
member [] x = False
member (y:ys) x = x == y || member ys x

  -- zip1 takes lists of lists, and rearranges them so that all the first
  -- elements are in the first list, all the second in the second and so on.
zip1 :: (Eq a) => [[a]] -> [[a]]
zip1 xs | member xs [] = []
zip1 xs = (map head xs):(zip1 (map tail xs))

  -- takes two lists and makes a list of tuples.
zip2 :: [a] -> [b] -> [(a,b)]
zip2=zip

  -- rept takes a function and a list of elements, and applies the function
  -- n-1 times to the n-th element
rept :: (a->a) -> a -> [a]
rept f x =  x:(rept f (f x))

  -- replicate creates an list n elements long of a, with the function
  -- applies to the n-th element n-1 times.
replicate :: Int -> (a->a->a) -> a -> a -> a
replicate 0 f zero a = zero
replicate 1 f zero a = a
replicate (n+2) f zero a = f a (replicate (n+1) f zero a)

  -- mapc is a map function for lists of functions (behaviours)
mapc :: (a->b) -> [c->a] -> [c->b]
mapc f as = [f.a | a <- as]


-- FUNCTIONS OVER SEQUENCES:

  -- append takes a list of lists, and makes them into one giant happy list.
append :: [[a]] -> [a]
append = foldr (++) []

  -- flatten takes a list of lists of tuples and gives one giant happy list
  -- of single elements back.
flatten:: [[(a,a)]] -> [a]
flatten s = foldr f []  (append s)
            where f (x,y) [] = [x,y]
                  f (x,y) (z:zs) = x:y:(z:zs)

  -- rptseq takes a list of elements and applies a function to them,
  -- n-1 times for the n-th element, but using map 
rptseq :: (a->a) -> [a] -> [a]
rptseq f [] = []
rptseq f (x:xs) = x:rptseq f (map f xs)

  -- osc takes a list, and makes sure it oscillates. If the head is 
  -- equal to the tail, it simply repeats the sequence infinitely. If
  -- the head is not equal to the tail, it adds the sequence then adds
  -- the reversed sequence minus the first and last elements, and then repeats
osc :: [a] -> [a]
osc s  | (length s) == 0 = []
osc s  | (length s) == 1 = head s: osc s
osc s           = (s ++ (((tail.reverse).tail) s)) ++ (osc s)




