{-******************************************************************
  MODULE R_INBETWEEN

    This module takes care of interpolation functions. Basically,
  given two Pics, inbetween will give you a movie gradually 
  converting from one Pic to the other Pic, using linear interpolation.
    Tween will take two Movies, and append them, interpolating n
  frames between the last Pic of the first Movie and the first Pic of
  the last Movie.      

******************************************************************-}

module R_Inbetween (inbetween,tween) where

import R_Ptypes
import R_Utility
import R_Picture
import R_Behaviour

  -- inbetween takes an int and two Pics, and interpolates n Pics
  -- of interpolated Pics. 
inbetween :: Int -> Pic -> Pic -> Movie
inbetween n p1 p2 | (length p1 == length p2) = 
                       ((zip1.(map (inbetweenp n))).zip1) [p1,p2]
inbetween  n p1 p2       = inbetween n [(col,p1')] [(col,p2')]
                          where p1' = concat [ vs | (c,vs) <- p1]
                                p2' = concat [ vs | (c,vs) <- p2]
                                col = head [ c | (c,vs) <- p1 ]
                      
  -- inbetweenp takes a list of 2 Polygons ([[Vec]]) and returns a 
  -- sequence of interpolated Polygons. Should the Number of vertices 
  -- of one Polygon be less than those in the other, it splits it so 
  -- as to have two Polygons of the same length.
inbetweenp :: Int -> Pic -> Pic
inbetweenp n  [(c1,vs),(c2,ws)] = 
   if ((length vs) < (length ws)) then  
         inbetween1 (split (length ws) (c1,vs)) (c2,ws) 0 n
   else if ((length vs) > (length ws)) then
         inbetween1 (c1,vs) (split (length vs) (c2,ws)) 0 n
   else inbetween1 (c1,vs) (c2,ws) 0 n
                         

  -- inbetween1 returns a sequence of interpolated Polygons.
inbetween1 :: Poly -> Poly -> Int -> Int -> Pic
inbetween1 p1 p2 m n | m>n || n<=0 = []
inbetween1 p1 p2 m n               = inbetween2 p1 p2 m n 
                                     :inbetween1 p1 p2 (m+1) n

  -- inbetween2 returns ONE of the required sequence of 
  -- interpolated Polygons.
inbetween2 :: Poly -> Poly -> Int -> Int -> Poly
inbetween2 (c1,vs) (c2,ws) p q = (c1, map (partway p q) (zip1 [vs,ws]))
          
  -- split splits up a Polygon so as to have the given #vertices.
split :: Int -> Poly -> Poly
split n (c,vs) = (c, split' n vs)

split' :: Int -> [Vec] -> [Vec]
split' n vs | n<= (length vs) = vs
split' n vs = if (n>double) then
                     split' n (split' double vs)
              else 
                     v1:(mid v1 v2):(split' (n-2) (v2:vss))
              where double = 2*((length vs)) - 1
                    (v1:v2:vss) = vs

                           
  -- tween will interpolate n Pics transforming the last Pic of
  -- the first Movie into the first Pic of the second Movie, and
  -- then run the second Movie    
tween :: Int -> Movie -> Movie -> Movie         
tween n m1 []   = m1
tween n m1 m2  = m1 ++ (inbetween n (last m1) (head m2')) ++ (tail m2')       
            where m2' = apply (mov (repeat v)) m2 
                  v = vmin (orig_Pic (last m1)) (orig_Pic (head m2))

  -- tweens will take a list of Movies and append them all, interpolating
  -- n frames between every Movie.
tweens :: Int -> [Movie] -> Movie
tweens n = foldr (tween n) [] 


