{-****************************************************************
  MODULE R_DEFAULTS

    This module uses the R_Behaviour module to define convient and
  easy to use behaviours. These aren't very sophistated, but they
  can be used to quickly animate a movie. For more sophistated
  animation, a similiar library of sophistocated personnalized 
  functions can be created.

******************************************************************-}

module R_Defaults (big, huge, bigger, smaller, ccw, cw, 
                   up, down, left, right,small,tiny)
where

import R_Ptypes
import R_Constants
import R_Utility
import R_Picture
import R_Behaviour


  -- big scales everything up by the scaleunit (now 12/11ths)
big :: Behaviour
big = [scale_Pic x | x <- [scaleunit,scaleunit..]]

  -- huge scales everything up by twice the scaleunit (24/11ths)
huge :: Behaviour
huge= [scale_Pic x | x <- [scaleunit*2,(scaleunit*2)..]]
  
  -- small scales everything down by 10/11ths
small :: Behaviour
small = [scale_Pic x | x <- [s,s..]]
        where s = 10
  
  -- tiny scales everything down by 5/11ths
tiny :: Behaviour
tiny  = [scale_Pic x | x <- [s,s..]]
        where s = 5
  
  -- bigger causes the Pics to be scaled up by 12/11ths,24/11ths,36/11ths
  -- and so on, everincreasing.
bigger :: Behaviour
bigger = [scale_Pic x | x <- (rept (\x -> div (x*scaleunit) 11) 1)]
  
  -- smaller causes the Pics to be scaled downwards in ever decreasing 
  -- amounts.
smaller :: Behaviour
smaller = [scale_Pic x | x <- (rept (\x -> div (x*10) 11) 1)]
  
  -- a hardwired version of ccw that rotates the Pics by one rotunit
  -- more every Pic, counterclockwise.
ccw :: Behaviour
ccw = [twist_Pic x | x <- [0.0,rotunit..]]
  
  -- same as ccw, but rotates the Pics clockwise
cw :: Behaviour
cw = [twist_Pic x | x <- [0.0,-rotunit..]]
  
  -- moves the Pic up by one more unit every Pic.
up :: Behaviour
up    = [mov_Pic (x,y) | (x,y)<- zip2 [0,0..] [0,unit..]]

  -- moves the Pic down by one more unit every Pic.  
down :: Behaviour
down  = [mov_Pic (x,y) | (x,y)<-zip2 [0,0..] [0,-unit]] 
  
  -- moves the Pic left by one more unit every Pic.
left :: Behaviour
left  = [mov_Pic (x,y) | (x,y)<- zip2 [0,-unit..] [0,0..]] 
  
  -- moves the Pic right by one more unit every Pic.
right :: Behaviour
right = [mov_Pic (x,y) | (x,y)<- zip2 [0,unit..] [0,0..]] 
  

