{-************************************************************
   MODULE R_PICTURE
 
     This module contains all the functions that can be used to manipulate
   Pic's. The user will probably never use any of these functions. They
   are used by Behaviours and such higher-order functions, which apply
   these routines to all the Pic's in the list.
     
*************************************************************-}

module R_Picture (close_Pic, ht_Pic, wid_Pic, orig_Pic,
                  overlay_Pic, put_Pic, over_Pic, above_Pic, beside_Pic,
                  map_Pic,beside2_Pic,
                  scale_Pic, scale_rel_Pic, mov_Pic, rot_Pic, twist_Pic,
                  twist_Pic', flipx_Pic, flipy_Pic, flip_Pic, {- flock_Pic, -}
                  set_Color_Pic,
                  to_orig_Pic,
		  movto_Pic
                  ) where

import R_Ptypes
import R_Constants
import R_Utility

  -- close_Pic makes sure that the polygon is closed
close_Pic:: Pic -> Pic
close_Pic p = map close_Poly p
              where
              close_Poly (c,ply) | (head ply) == (last ply) = (c,ply)
              close_Poly (c,ply)       = (c,ply++(tail (reverse ply)))

  --these functions find the max and min x and y coordinates of a Pic
maxx :: Pic -> Int
maxx p = reduce max [x | (c,q) <- p, (x,y) <- q]

minx :: Pic -> Int
minx p = reduce min [x | (c,q) <- p, (x,y) <- q]

maxy :: Pic -> Int
maxy p = reduce max [y | (c,q) <- p, (x,y) <- q]

miny :: Pic -> Int
miny p = reduce min [y | (c,q) <- p, (x,y) <- q]

  -- these functions find the height, width and origin of a Pic
ht_Pic :: Pic -> Int
ht_Pic p = (maxy p) - (miny p)

wid_Pic :: Pic -> Int
wid_Pic p = (maxx p) - (minx p)

orig_Pic:: Pic -> Vec
orig_Pic p = ( (maxx p + minx p) `div` 2, (maxy p + miny p) `div` 2 )

-- PICTURE COMBINING OPERATIONS:
  
  -- overlay_Pic just takes 2 Pics and puts them together into one
overlay_Pic:: Pic -> Pic -> Pic
overlay_Pic p q = p ++ q

  -- put_Pic overlays the Pics, offsetting the first Pic by a vector
  -- amount from the origin of the second
put_Pic:: Vec -> Pic -> Pic -> Pic
put_Pic v p q = overlay_Pic
                     (movto_Pic (vplus (orig_Pic q) v) p )
                     q

  -- over_Pic puts one Pic directly on top of the other
over_Pic:: Pic -> Pic -> Pic
over_Pic p q = put_Pic (0,0) p q

  -- above_Pic puts the first Pic on top of the second
above_Pic:: Pic -> Pic -> Pic
above_Pic p q = put_Pic (0,(((ht_Pic q) + (ht_Pic p)) `div` 2)) p q

  -- beside_Pic puts the first Pic beside the second. The width of
  -- the Pic is defined as the max x minus the min x, so a moving
  -- figure will stand still in this implementation
beside_Pic:: Pic -> Pic -> Pic
beside_Pic p q = put_Pic (((wid_Pic q)+(wid_Pic p)) `div` 2, 0) p q

  -- beside2_Pic puts the first Pic beside the second, without 
  -- shifting to the width of the Pic. It uses the absolute coordinates.
beside2_Pic:: Pic -> Pic -> Pic
beside2_Pic p q = put ((wid_Pic q), 0) p q
     where put v p q = overlay_Pic (mov_Pic v p) q


  -- The following maps a given function over the Vector-list of each Polygon:
map_Pic:: (Vec -> Vec) -> Pic -> Pic
map_Pic f p = map f' p
              where f' (c,vs) = (c, map f vs)

-- THE GEOMETRIC TRANSFORMATIONS:

  -- scales the Pic by r, where r is in units of 11th. ie r=1, the Pic is
  -- scaled by 1/11 to its origin. 
scale_Pic :: Int -> Pic -> Pic
scale_Pic r p
   = map_Pic (scalep r) p
     where scalep r (v1,v2) = (div ((r*(v1-dx))+dx) 11,div ((r*(v2-dy))+dy) 11)
           dx = fst (orig_Pic p)
           dy = snd (orig_Pic p)

  -- this is another scaling function, but it centers the image at the Vec
scale_rel_Pic :: Vec -> Int -> Pic -> Pic
scale_rel_Pic v r
   = map_Pic (scalep r)
     where scalep r (v1,v2) = (div ((r*(v1-dx))+dx) 11,div ((r*(v2-dy))+dy) 11)
           dx = fst v
           dy = snd v

  -- moves a Pic by the vector amount
mov_Pic:: Vec -> Pic -> Pic
mov_Pic v = map_Pic (vplus v)

  -- moves a Pic to the vector
movto_Pic:: Vec -> Pic -> Pic
movto_Pic v p = mov_Pic (vmin v (orig_Pic p)) p

  -- moves the origin of the Pic to the lower left side of the Pic
to_orig_Pic:: Pic -> Pic
to_orig_Pic p = mov_Pic (-mx,-my) p
                where mx = minx p
                      my = miny p

  -- rotates the Pic about the Vector by theta
rot_Pic :: Vec -> Float -> Pic -> Pic
rot_Pic (a,b) theta
                   = map_Pic  (rotp (a,b) theta)
                     where rotp (a,b) t (v1,v2)
                             = vftov (a2+ (u * cos theta - v * sin theta),
                                      b2+ (u * sin theta + v * cos theta))
                                where u =  u1 -a2
                                      v =  u2 -b2
				      (u1,u2) = vtovf (v1,v2)
 				      (a2,b2) = vtovf (a,b)

  -- rotates a Pic about its origin by theta
twist_Pic :: Float -> Pic -> Pic
twist_Pic theta p = rot_Pic (orig_Pic p) theta p


  -- hardwired version of rot_Pic that runs faster by rotating a set
  -- unit, the rotunit, every time
rot_Pic':: Vec -> Pic -> Pic
rot_Pic' (a,b) = map_Pic (rotp (a,b))
                 where rotp (a,b) (v1,v2)
                         = vftov (a2+ (u * cosunit - v * sinunit),
                                  b2+ (u * sinunit + v * cosunit))
                            where u = u1-a2
                                  v = u2-b2
				  (u1,u2) = vtovf (v1,v2)
				  (a2,b2) = vtovf (a,b)

  -- hardwired version of twist_Pic that runs faster using rot_Pic'
twist_Pic':: Pic -> Pic
twist_Pic' p = rot_Pic' (orig_Pic p) p

  -- flips the Pic about the line x=n (x-coordinates change)
flipx_Pic :: Int -> Pic -> Pic 
flipx_Pic n  = map_Pic (flipvx n)
               where
               flipvx n (a,b) = (2*(n-a)+a,b)

  -- flips the Pic about the line y=n (y-coordinates change)
flipy_Pic :: Int -> Pic -> Pic 
flipy_Pic n = map_Pic (flipvy n)
              where
              flipvy n (a,b) = (a, 2*(n-b)+b)

  -- flips the Pic about its own x origin.
flip_Pic:: Pic -> Pic
flip_Pic p = map_Pic (flipvx x) p
             where (x,y) = orig_Pic p
                   flipvx n (a,b) = (2*(n-a)+a,b)

  -- copies the Pic into another Pic n*n times in an n by n array pattern
flock_Pic :: Int -> Pic -> Pic
flock_Pic 1 p = p
flock_Pic (n+2) p = beside_Pic (flock_Pic (n-1) p) (row n p)
                    where row n p = replicate n above_Pic nullpic p

  -- changes the color of the Pic
set_Color_Pic:: Color -> Pic -> Pic
set_Color_Pic c p = map f p
                    where f (c',vs) = (c,vs)

