{-**************************************************************
  MODULE R_CURVE

    This module produces sequences of numbers to be used by
  Behaviours. The sequences used for moving or scaling can
  be produced here, in either linear sequences or accelerating
  and decelerating sequences.
    The acceleration functions produce floats, so the vftov function
  would have to be used to convert floating point vectors to integer
  vectors.

***************************************************************-}

module R_Curve(lnr,hold, acc, dec, accdec, decacc) where

import R_Ptypes
import R_Constants
import R_Utility
import R_Picture
import R_Behaviour

  -- lnr takes the start, finish and the number of intervals and
  -- produces a linear list of ints going from the start to finish.
lnr :: Int -> Int -> Int ->[Int]
lnr start fin n = take n [start,(start+step)..]
			where step = ((fin-start)`div`(n-1))

  -- hold produces an infinite number of ints starting at v, modified
  -- by step every time.
hold :: Int -> Int -> [Int]  
hold v step  = [v,v+step..]

  -- acc accelerates from 0 to the max in n steps.
acc :: Int -> Int -> Int -> [Int]
acc min max n = min:acc' min (max-min) n 1 

acc' ::  Int -> Int -> Int -> Int -> [Int]
acc' min max n c | (c>n) = []
acc' min max n c         = (min + (((max*c*c) `div` (n*n)))) 
                           : (acc' min max n (c+1)) 


  -- dec decelerates from the max to 0 in n steps.
dec :: Int -> Int -> Int -> [Int]
dec min max n = reverse (acc min max n)
  
  -- accdec accelerates from start up to max and back to fin, in an steps
  -- accelerating and dn steps decelerating
accdec :: Int -> Int -> Int -> Int -> Int -> [Int]
accdec start max fin an dn = (acc start max an)++(tail (dec fin max dn))

  -- decacc decelerates from start to min in dn steps and then accelerates
  -- back up to fin in an more steps
decacc :: Int -> Int -> Int -> Int -> Int -> [Int]
decacc start min fin dn an = (dec min start dn)++(tail (acc min fin an))





