{-*********************************************************************
  MODULE R_MOVIE

    This module contains necessary functions for editing Movies. There
  are several that give information on a Movie, such as the heights or
  wirdths of its Pics. The others all deal with the various ways of
  combining various Movies into one Movie, a vital set of functions.

*********************************************************************-}

module R_Movie (ht, wid, orig,
                above, rABOVE, beside, rBESIDE,rBESIDE2, over, rOVER,
                overlay, rOVERLAY, pUT,
                uncurry, curry
                ) where

import R_Ptypes
import R_Constants
import R_Utility
import R_Picture

  -- takes a function and a list and returns a new list of element operated
  -- on by the function.
promote:: (a->b)->[a]->[b]
promote f []     = []
promote f [p]    = f p:promote f [p]
promote f (p:ps) = f p:promote f ps

  -- promote1 takes a function that analyzes a Pic, and then applies it
  -- to analyse a movie, returning a list.
promote1:: (Pic->a) -> Movie -> [a]
promote1 f ps = [f p | p <- ps]

  -- ht takes a Movie and returns a list of the heights of the Pics
ht :: Movie -> [Int]
ht   = promote1 ht_Pic

  -- wid takes a Movie and returns a list of the widths of the Pics
wid :: Movie -> [Int]
wid  = promote1 wid_Pic

  -- orig takes a Movie and returns a list of vectors that are the
  -- origins of the Pics
orig:: Movie -> [Vec]
orig = promote1 orig_Pic

  -- promote2 takes a function accepting an element and a Pic, and
  -- applies the function to the Movie and list, producing a new Movie
promote2:: (a->Pic->Pic) -> [a] -> Movie -> Movie
promote2 f ps qs = [f p q | (p,q) <- zip2 ps qs]

  -- takes two Movies and puts them above one another
above:: Movie -> Movie -> Movie
above = promote2 above_Pic

  -- takes a list of Movies and puts them all above one another
rABOVE:: [Movie] -> Movie
rABOVE = reduce above

  -- takes two Movies and puts them beside one another
beside:: Movie -> Movie -> Movie
beside = promote2 beside_Pic

  -- takes a list of Movies and puts them all beside one another
rBESIDE:: [Movie] -> Movie
rBESIDE = reduce beside

  -- same as beside, but with absolute coordinates.
beside2:: Movie -> Movie -> Movie
beside2 = promote2 beside2_Pic

  -- same as rBESIDE, but with absolute coordinates.
rBESIDE2:: [Movie] -> Movie
rBESIDE2 = reduce beside2

  -- puts one Movie on top of the other Movie
over:: Movie -> Movie -> Movie
over = promote2 over_Pic

  -- takes a list of Movies, and puts the n-th on top of the first
  -- through 9n-1)th.
rOVER:: [Movie] -> Movie
rOVER = reduce over

  -- just overlays the two Movies by appending the Pics.
overlay:: Movie -> Movie -> Movie
overlay = promote2 overlay_Pic

  -- overlays a list of Movies by appending the Pics
rOVERLAY:: [Movie] -> Movie
rOVERLAY = reduce overlay

  -- promote3 takes a function that takes two items and a Pic and 
  -- returns a Pic, and then applies it to two input lists and a Movie,
  -- producing a new Movie.
promote3:: (a->b->Pic->Pic) -> [a] -> [b] -> Movie -> Movie
promote3 f ps qs rs = [f p q r | (p,q,r) <- zip3 ps qs rs]

  -- pUT takes a list of Vectors, and puts each Pic of the first Movie
  -- in the location of the corresponding vector, on top  of the Pic of
  -- the second Movie, and returns that list as a new Movie.
pUT:: [Vec] -> Movie -> Movie -> Movie
pUT = promote3 put_Pic

  -- uncurry takes a function that takes two elements and a list of
  -- two elements and applies the function to them.
uncurry:: (a->a->b) -> [a] -> b
uncurry f [a,b] = f a b

  -- curry takes a function that takes a list, and two elements, and
  -- then applies the function to the elements in a list.
curry:: ([a]->b) -> a -> a -> b
curry f a b = f [a,b]

