{-**********************************************************************
  MODULE R_BEHAVIOUR

    This module defines the basic Behaviours available to manipulate
  Movies. These functions can either be used directly, or used to
  easily create personnalized Behaviours (see R_Defaults).
    There are the Behaviours that affect one Movie, which are mov,movto
  circ_mov,scale,scale_rel,rot,flip and set_color. These change some 
  aspect of the movie over time.
    There are functions that combine several movies into one, namely
  bseq,bSeq,bpar and bPar.
    Some functions modify the Behaviours. These are do, rpt and forever.
  They put limits on how long the Behaviour is. 
    Finally, there are the functions that apply the Behaviours to a Movie.
  These are apply and while. Apply applies a Behaviour to a Movie until
  it runs out of Movie or Behaviour. While takes a conditional and
  applies the Behaviour to it until that condition is fullfilled.

***********************************************************************-}

module R_Behaviour (mov,movto,circ_mov,scale,scale_rel,rot,flipb,
		    set_color,
   		    bseq,bSeq,bpar,bPar,
		    do,rpt,forever,
		    apply,while )  where

import R_Ptypes
import R_Utility
import R_Picture

  -- takes a Pic to Pic and makes an infinite list Behaviour out of it	    
makeb1 :: (Pic->Pic) -> Behaviour
makeb1 f = f : makeb1 f

  -- takes a movie and flips it around the x-origin using flip_Pic
flipb :: Behaviour
flipb = makeb1 flip_Pic

  -- twist makes twist_Pic into a Behaviour, rotating the image by rotunit
twist' :: Behaviour
twist' = makeb1 twist_Pic'

  -- makeb2 makes a Behaviour out of a function that takes a list and a 
  -- function and outputs a Behaviour.
makeb2 :: (a->Pic->Pic) -> [a] -> Behaviour
makeb2 f [] = []
makeb2 f (v:vs) = f v : makeb2 f vs

  -- mov takes a list of Vec's and applies each Pic-to-Pic in the Behaviour
  -- list to its corresponding Vec, and gives back a new Behaviour
mov :: [Vec] ->Behaviour
mov = makeb2 mov_Pic

  -- movto creates a list of Pic-to-Pic Behaviours that move each Pic to its 
  -- corresponding Vec
movto :: [Vec] -> Behaviour
movto = makeb2 movto_Pic

  -- produces a Behaviour that produces movement in a circle, taking
  -- the radius and the increment as arguments.
circ_mov :: Float -> Float -> Behaviour
circ_mov r inc = mov (map (vmin' (head vs')) vs')
                    where vs = [ (r*(cos theta),r*(sin theta)) |
                               theta <- gen inc 0.0  ]
                          vmin' x y = vmin y x
                          vs' = map vftov vs

gen :: Float -> Float -> [Float]
gen b c = c : (gen b (c+b) )


  -- scale outputs a list of Pic-to-Pic's that scale according to its 
  -- corresponding Int in the input list
scale :: [Int] -> Behaviour
scale = makeb2 scale_Pic

  -- scale_rel does the same thing, but centers on the lower-left corner of
  -- the image
scale_rel :: Vec -> [Int] -> Behaviour
scale_rel v = makeb2 (scale_rel_Pic v)

  -- twist outputs a list of Behaviours that rotate each pick by its 
  -- corresponding Float in the list
twist :: [Float] -> Behaviour
twist = makeb2 twist_Pic

  -- set_color takes a list of Colors, and returns a list of Pic-to-Pic's
  -- that change to the corresponding color in the list
set_color :: [Color] -> Behaviour
set_color = makeb2 set_Color_Pic

  -- makeb3 takes a function with two inputs, and two input lists and
  -- returns a behaviour made up of functions with inputs fromt the lists
makeb3 :: (a->b->Pic->Pic) -> [a] -> [b] -> Behaviour
makeb3 f [] (p:ps) = []
makeb3 f (v:vs) [] = []
makeb3 f (v:vs) (p:ps) = f v p : makeb3 f vs ps

  -- rot produces behaviours rotating by the Float, around the point
  -- of the Vec, both provided by lists.
rot :: [Vec] -> [Float] -> Behaviour
rot = makeb3 rot_Pic

  -- bseq takes two Behaviours and combines them into one, in sequence. 
  -- It first applies all of the first Behaviour, then all of the second
bseq :: Behaviour -> Behaviour -> Behaviour
bseq ps [] = []
bseq [] qs = []
bseq ps qs = ps ++ (mapc (last ps) qs)

  -- bSeq takes a list of Behaviour and makes them into one Behaviour, in
  -- sequence.
bSeq :: [Behaviour] -> Behaviour
bSeq = reduce bseq

  -- bpar takes two behaviours and applies them both simultaneously,
  -- producing a list of Pic-to-Pic's, each one made up of a function
  -- from the first list combined with a function from the second list
bpar :: Behaviour -> Behaviour -> Behaviour
bpar [] (q:qs) = []
bpar (p:ps) [] = []
bpar (p:ps) (q:qs) = (p.q):(bpar ps qs)

  -- bPar takes a list of Behaviours and makes them all into one Behaviour,
  -- in paralell
bPar :: [Behaviour] -> Behaviour
bPar = reduce bpar

  -- takes the first n POic-to-Pics in a Behaviour and returns that Behaviour 
do :: Int -> Behaviour -> Behaviour
do n f = take n f

  -- applies bseq to the list of behaviours, so that the nth element of
  -- the returned list has n-1 behaviours in it, applied in sequence
rpt :: Int -> Behaviour -> Behaviour
rpt n f = replicate n bseq [] f

  -- takes the behaviour and applies all the behaviours up the nth element
  -- to the nth element, in an infinite list
forever :: Behaviour -> Behaviour
forever f = bseq f (forever f)

  -- takes a behaviour, applies each from to a Pic in a Movie and returns
  -- the new Movie
apply :: Behaviour -> Movie -> Movie
apply [] ms = []
apply fs [] = []
apply (f:fs) (m:ms) = (f m):(apply fs ms)

  -- applies the Behaviour to the Movie until the condition is fullfilled,
  -- then returns the movie to that point
while :: (Pic -> Bool) -> Behaviour -> Movie -> Movie
while c [] ms = []
while c fs [] = []
while c (f:fs) (m:ms) = if (c m) then ( (f m):(while c fs ms))
                        else []


