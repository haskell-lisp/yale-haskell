{-*****************************************************************
  MODULE R_SHAPES
  
    This modules produces Pic's of boxes and triangles to help build
  Pic's to animate.    
    
******************************************************************-}

module R_Shapes (box, tri, circ_mov, circ) where

import R_Ptypes
import R_Utility
import R_Picture
import R_Behaviour

  -- box takes four three ints, the color, width and height of the box and
  -- returns a Pic of a box
box :: Int -> Int -> Int -> Pic
box c width height= [(c,[(0,0),(width,0),(width,height),(0,height),(0,0)])]

  -- tri takes a color and three vectors, and returns a Pic of a triangle
  -- with the vectors as vertices
tri:: Color -> Vec -> Vec -> Vec -> Pic
tri c (x1,y1) (x2,y2) (x3,y3) = [(c,[(x1,y1),(x2,y2),(x3,y3),(x1,y1)])]


  -- circ takes a color, the radius
circ :: Color -> Int -> Int -> Pic
circ c r inc = [(c,(r+r,r):(circ' r' inc' 1.0))]
               where r' = (fromIntegral r)
                     inc' = (fromIntegral inc)

circ' :: Float -> Float -> Float  -> [Vec]
circ' r inc c | c>inc = []
circ' r inc c         = vftov (x+r,y+r) : (circ' r inc (c+1.0))
                	where x = r*(cos((2*c*pi)/inc))
                      	      y = r*(sin((2*c*pi)/inc))
                    
