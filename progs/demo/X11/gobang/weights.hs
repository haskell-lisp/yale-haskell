module Weights where

import Xlib
import Utilities

xlookup :: XMArray Int -> Int -> Int -> IO Int
xlookup keyboard x y =
      if (x < 1 || x > 19 || y < 1 || y > 19) 
      then returnIO (-2)
      else xMArrayLookup keyboard ((x-1)*19+(y-1))


draw_unit :: XMArray Int -> XMArray Int -> XMArray Int -> Int -> Int  -> IO()
draw_unit keyboard weight1 weight2 x y = 
  let 
    update_weight :: XMArray Int->Int->Int->Int->Int->Int->Int->IO()
    update_weight weight counter player x y incr_x incr_y 
      | x>=1 && x<=19 && y>=1 && y<=19 && counter<=4 = 
          cpt_weight x y player `thenIO` \wt -> 
            xMArrayUpdate weight ((x-1)*19+(y-1)) wt `thenIO` \() ->
              update_weight weight (counter+1) player (x+incr_x) (y+incr_y)
	                    incr_x incr_y
      | otherwise = returnIO ()
----------------------------------------------------------------------------

    pattern0 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
    pattern0 a b c d e p | a==p && b==p && c==p && d==p && e==p = True
	                 | otherwise                            = False
----------------------------------------------------------------------------

    pattern1 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool 
    pattern1 a b c d e f p  | (a==0) && (b==p) && (c==p) && (d==p) && (e==p) &&
                              (f==0)     = True
	       		    | otherwise  = False     
----------------------------------------------------------------------------
 
    pattern2 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool  
    pattern2 a b c d e p | (a==0 && b==p && c==p && d==p && e==p)||
                           (a==p && b==p && c==p && d==p && e==0) = True 
			 | otherwise                              = False     
----------------------------------------------------------------------------
           
    pattern3 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool  
    pattern3 a b c d e p | (a==0 && b==p && c==p && d==p && e==0) = True
                         | otherwise                              = False 
----------------------------------------------------------------------------
           
    pattern4 :: Int -> Int -> Int -> Int -> Int ->  Bool  
    pattern4 a b c d p | (a==0 && b==p && c==p && d==p) ||
                         (a==p && b==p && c==p && d==0) = True
                       | otherwise                      = False      
----------------------------------------------------------------------------
           
    pattern5 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool  
    pattern5 a b c d e f p  | (a==0 && b==p && c==p && d==0 && e==p && 
                               f==0) ||
                              (a==0 && b==p && c==0 && d==p && e==p &&
                               f==0)    = True
			    | otherwise = False     
----------------------------------------------------------------------------
           
    pattern6 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool  
    pattern6 a b c d e p | (a==0 && b==p && c==p && d==0 && e==p) ||
                           (a==0 && b==p && c==0 && d==p && e==p) || 
                           (a==p && b==p && c==0 && d==p && e==0) || 
                           (a==p && b==0 && c==p && d==p && e==0) = True
			 | otherwise = False     
----------------------------------------------------------------------------
           
    pattern7 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int-> Bool
    pattern7 a b c d e f g p | (a==0 && b==p && c==0 && d==p && e==0 &&
                                 f==p && g==0) = True
			     | otherwise       = False     
----------------------------------------------------------------------------
           
    pattern8 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool  
    pattern8 a b c d e f p | (a==0 && b==p && c==0 && d==p && e==0 &&
                              f==p) ||
                             (a==p && b==0 && c==p && d==0 && e==p &&
		              f==0) = True 
                           | otherwise = False     
----------------------------------------------------------------------------
           
    pattern9 :: Int -> Int -> Int -> Int -> Int -> Bool  
    pattern9 a b c d p | (a==0 && b==p && c==p && d==0) = True
                       | otherwise                      = False     
----------------------------------------------------------------------------
           
    pattern10 :: Int -> Int -> Int -> Int -> Bool  
    pattern10 a b c p | (a==0 && b==p && c==p) ||
                        (a==p && b==p && c==0) = True
                      | otherwise              = False         
----------------------------------------------------------------------------
           
    pattern11 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool  
    pattern11 a b c d e p | (a==0 && b==p && c==0 && d==p && e==0) = True
                          | otherwise                              = False     
----------------------------------------------------------------------------
           
    pattern12 :: Int -> Int -> Int -> Int -> Int -> Bool  
    pattern12 a b c d p | (a==0 && b==p && c==0 && d==p) ||
                          (a==p && b==0 && c==p && d==0) = True
                        | otherwise                      = False   
----------------------------------------------------------------------------
 
    direct1 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> 
               Int -> Int -> Int -> Int -> Int -> Int
    direct1 x y pl ptN1 ptN2 ptN3 ptN4 ptN5 pt ptP1 ptP2 ptP3 ptP4 ptP5
      | (pattern0  ptN4 ptN3 ptN2 ptN1 pt pl) ||
        (pattern0  ptN3 ptN2 ptN1 pt ptP1 pl) ||
    	(pattern0  ptN2 ptN1 pt ptP1 ptP2 pl) ||
	(pattern0  ptN1 pt ptP1 ptP2 ptP3 pl) ||
        (pattern0  pt ptP1 ptP2 ptP3 ptP4 pl) = 200
      | (pattern1  ptN4 ptN3 ptN2 ptN1 pt ptP1 pl) ||
        (pattern1  ptN3 ptN2 ptN1 pt ptP1 ptP2 pl) ||
    	(pattern1  ptN2 ptN1 pt ptP1 ptP2 ptP3 pl) ||
	(pattern1  ptN1 pt ptP1 ptP2 ptP3 ptP4 pl) = 40
      | (pattern2  ptN4 ptN3 ptN2 ptN1 pt pl) ||
        (pattern2  ptN3 ptN2 ptN1 pt ptP1 pl) ||
    	(pattern2  ptN2 ptN1 pt ptP1 ptP2 pl) ||
	(pattern2  ptN1 pt ptP1 ptP2 ptP3 pl) = 13
      | (pattern3  ptN3 ptN2 ptN1 pt ptP1 pl) ||
        (pattern3  ptN2 ptN1 pt ptP1 ptP2 pl) ||
        (pattern3  ptN1 pt ptP1 ptP2 ptP3 pl) = 10
      | (pattern4  ptN3 ptN2 ptN1 pt pl) ||
        (pattern4  ptN2 ptN1 pt ptP1 pl) ||
        (pattern4  ptN1 pt ptP1 ptP2 pl) = 8
      | (pattern5  ptN4 ptN3 ptN2 ptN1 pt ptP1 pl) ||
        (pattern5  ptN3 ptN2 ptN1 pt ptP1 ptP2 pl) ||
        (pattern5  ptN2 ptN1 pt ptP1 ptP2 ptP3 pl) || 
        (pattern5  ptN1 pt ptP1 ptP2 ptP3 ptP4 pl) = 9
      | (pattern6  ptN4 ptN3 ptN2 ptN1 pt pl) ||
        (pattern6  ptN3 ptN2 ptN1 pt ptP1 pl) ||
        (pattern6  ptN2 ptN1 pt ptP1 ptP2 pl) ||
        (pattern6  ptN1 pt ptP1 ptP2 ptP3 pl) = 7
      | (pattern7  ptN5 ptN4 ptN3 ptN2 ptN1 pt ptP1 pl) ||
        (pattern7  ptN4 ptN3 ptN2 ptN1 pt ptP1 ptP2 pl) ||
	(pattern7  ptN3 ptN2 ptN1 pt ptP1 ptP2 ptP3 pl) || 
        (pattern7  ptN2 ptN1 pt ptP1 ptP2 ptP3 ptP4 pl) ||
        (pattern7  ptN1 pt ptP1 ptP2 ptP3 ptP4 ptP5 pl) = 6   
      | (pattern8  ptN5 ptN4 ptN3 ptN2 ptN1 pt pl) ||
        (pattern8  ptN4 ptN3 ptN2 ptN1 pt ptP1 pl) ||
        (pattern8  ptN3 ptN2 ptN1 pt ptP1 ptP2 pl) ||
        (pattern8  ptN2 ptN1 pt ptP1 ptP2 ptP3 pl) ||
        (pattern8  ptN1 pt ptP1 ptP2 ptP3 ptP4 pl) || 
        (pattern8  pt ptP1 ptP2 ptP3 ptP4 ptP5 pl) = 5
      | (pattern9  ptN2 ptN1 pt ptP1 pl) || 
        (pattern9  ptN1 pt ptP1 ptP2 pl) = 4
      | (pattern10 ptN2 ptN1 pt pl) ||
        (pattern10 ptN1 pt ptP1 pl) ||
        (pattern10 pt ptP1 ptP2 pl) = 2
      | (pattern11 ptN3 ptN2 ptN1 pt ptP1 pl) || 
        (pattern11 ptN2 ptN1 pt ptP1 ptP2 pl) ||
        (pattern11 ptN1 pt ptP1 ptP2 ptP3 pl) = 3
      | (pattern12 ptN3 ptN2 ptN1 pt pl) ||
        (pattern12 ptN2 ptN1 pt ptP1 pl) ||
        (pattern12 ptN1 pt ptP1 ptP2 pl) ||
        (pattern12 pt ptP1 ptP2 ptP3 pl) = 1
      | otherwise = 0
----------------------------------------------------------------------------

    direct2 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> 
               Int -> Int -> Int -> Int -> Int -> Int
    direct2 x y pl ptN1 ptN2 ptN3 ptN4 ptN5 pt ptP1 ptP2 ptP3 ptP4 ptP5
      | (pattern0  ptN4 ptN3 ptN2 ptN1 pt pl) ||
        (pattern0  ptN3 ptN2 ptN1 pt ptP1 pl) ||
    	(pattern0  ptN2 ptN1 pt ptP1 ptP2 pl) ||
	(pattern0  ptN1 pt ptP1 ptP2 ptP3 pl) ||
        (pattern0  pt ptP1 ptP2 ptP3 ptP4 pl) = 200
      | otherwise = 0
-----------------------------------------------------------------------------

    cpt_weight :: Int -> Int -> Int -> IO Int
    cpt_weight x y player = 
      xMArrayLookup keyboard ((x-1)*19+(y-1)) `thenIO` \(unit) -> 
      if (unit /= 0) 
        then returnIO (-1) 
        else xlookup keyboard x (y-1) `thenIO` \(xyN1) ->
             xlookup keyboard x (y-2) `thenIO` \(xyN2) ->
             xlookup keyboard x (y-3) `thenIO` \(xyN3) ->
	     xlookup keyboard x (y-4) `thenIO` \(xyN4) ->
	     xlookup keyboard x (y-5) `thenIO` \(xyN5) ->
	     xlookup keyboard x (y+1) `thenIO` \(xyP1) ->
	     xlookup keyboard x (y+2) `thenIO` \(xyP2) ->
	     xlookup keyboard x (y+3) `thenIO` \(xyP3) ->
	     xlookup keyboard x (y+4) `thenIO` \(xyP4) ->
	     xlookup keyboard x (y+5) `thenIO` \(xyP5) ->
	     xlookup keyboard (x-1) y `thenIO` \(xN1y) ->
	     xlookup keyboard (x-2) y `thenIO` \(xN2y) ->
             xlookup keyboard (x-3) y `thenIO` \(xN3y) ->
	     xlookup keyboard (x-4) y `thenIO` \(xN4y) ->
	     xlookup keyboard (x-5) y `thenIO` \(xN5y) ->
	     xlookup keyboard (x+1) y `thenIO` \(xP1y) ->
	     xlookup keyboard (x+2) y `thenIO` \(xP2y) ->
	     xlookup keyboard (x+3) y `thenIO` \(xP3y) ->
	     xlookup keyboard (x+4) y `thenIO` \(xP4y) ->
	     xlookup keyboard (x+5) y `thenIO` \(xP5y) ->
	     xlookup keyboard (x-1) (y-1) `thenIO` \(xN1yN1)->
             xlookup keyboard (x-2) (y-2) `thenIO` \(xN2yN2) ->
             xlookup keyboard (x-3) (y-3) `thenIO` \(xN3yN3) ->
             xlookup keyboard (x-4) (y-4) `thenIO` \(xN4yN4) ->
             xlookup keyboard (x-5) (y-5) `thenIO` \(xN5yN5) ->
             xlookup keyboard (x+1) (y+1) `thenIO` \(xP1yP1) ->
             xlookup keyboard (x+2) (y+2) `thenIO` \(xP2yP2) ->
             xlookup keyboard (x+3) (y+3) `thenIO` \(xP3yP3) ->
             xlookup keyboard (x+4) (y+4) `thenIO` \(xP4yP4) ->
             xlookup keyboard (x+5) (y+5) `thenIO` \(xP5yP5) ->
             xlookup keyboard (x-1) (y+1) `thenIO` \(xN1yP1) -> 
             xlookup keyboard (x-2) (y+2) `thenIO` \(xN2yP2) ->
             xlookup keyboard (x-3) (y+3) `thenIO` \(xN3yP3) -> 
             xlookup keyboard (x-4) (y+4) `thenIO` \(xN4yP4) -> 
             xlookup keyboard (x-5) (y+5) `thenIO` \(xN5yP5) -> 
             xlookup keyboard (x+1) (y-1) `thenIO` \(xP1yN1) -> 
             xlookup keyboard (x+2) (y-2) `thenIO` \(xP2yN2) -> 
             xlookup keyboard (x+3) (y-3) `thenIO` \(xP3yN3) -> 
             xlookup keyboard (x+4) (y-4) `thenIO` \(xP4yN4) -> 
             xlookup keyboard (x+5) (y-5) `thenIO` \(xP5yN5) ->
	     returnIO ( (direct1 x y player xyN1 xyN2 xyN3 xyN4 xyN5 player
	                         xyP1 xyP2 xyP3 xyP4 xyP5) + 
	                (direct1 x y player xN1y xN2y xN3y xN4y xN5y player
	                         xP1y xP2y xP3y xP4y xP5y) +
                        (direct1 x y player xN1yN1 xN2yN2 xN3yN3 xN4yN4 
			         xN5yN5 player xP1yP1 xP2yP2 xP3yP3 xP4yP4
				 xP5yP5) + 
	                (direct1 x y player xN1yP1 xN2yP2 xN3yP3 xN4yP4 
			         xN5yP5 player xP1yN1 xP2yN2 xP3yN3 xP4yN4
				 xP5yN5) )
-----------------------------------------------------------------------------

--                        | 1111 && no_block = 20
--			  | 1111 && one_block = 13
--			  | 111 && no_block = 10
--			  | 111 && one_block = 8
--			  | 11 1 or 1 11 && no_block = 9
--			  | 11 1 or 1 11 && one_block =7
--                        | 1 1 1 && no_block = 6
--			  | 1 1 1 && one_block= 5
--			  | 11 && no_block = 4
--			  | 11 && one_block =2
--			  | 1 1 && no_block =3
--			  | 1 1 && one_block=1

  in
    update_weight weight1 0 1 x y 1    1    `thenIO` \() ->
    update_weight weight2 0 2 x y 1    1    `thenIO` \() ->
    update_weight weight1 0 1 x y 1    (-1) `thenIO` \() ->
    update_weight weight2 0 2 x y 1    (-1) `thenIO` \() ->
    update_weight weight1 0 1 x y (-1) (-1) `thenIO` \() ->
    update_weight weight2 0 2 x y (-1) (-1) `thenIO` \() ->
    update_weight weight1 0 1 x y (-1) 1    `thenIO` \() ->  
    update_weight weight2 0 2 x y (-1) 1    `thenIO` \() ->    
    update_weight weight1 0 1 x y 0    1    `thenIO` \() ->
    update_weight weight2 0 2 x y 0    1    `thenIO` \() ->
    update_weight weight1 0 1 x y 0    (-1) `thenIO` \() ->
    update_weight weight2 0 2 x y 0    (-1) `thenIO` \() ->
    update_weight weight1 0 1 x y (-1) 0    `thenIO` \() ->
    update_weight weight2 0 2 x y (-1) 0    `thenIO` \() ->  
    update_weight weight1 0 1 x y 1    0    `thenIO` \() ->   
    update_weight weight2 0 2 x y 1    0    `thenIO` \() ->  
    returnIO ()


human_unit :: XMArray Int -> Int -> Int  -> IO(Bool)
human_unit keyboard x y =
  let    
    pattern0 :: Int -> Int -> Int -> Int -> Int -> Bool
    pattern0 a b c d e | a==b && b==c && c==d && d==e = True
	               | otherwise                    = False    
			 
    direct3 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> 
               Int
    direct3 ptN1 ptN2 ptN3 ptN4 pt ptP1 ptP2 ptP3 ptP4 
      | (pattern0  ptN4 ptN3 ptN2 ptN1 pt) ||
        (pattern0  ptN3 ptN2 ptN1 pt ptP1) ||
    	(pattern0  ptN2 ptN1 pt ptP1 ptP2) ||
	(pattern0  ptN1 pt ptP1 ptP2 ptP3) ||
        (pattern0  pt ptP1 ptP2 ptP3 ptP4) = 200
      | otherwise = 0
  in
    xlookup keyboard x y `thenIO` \(xy) ->
    xlookup keyboard x (y-1) `thenIO` \(xyN1) ->
    xlookup keyboard x (y-2) `thenIO` \(xyN2) ->
    xlookup keyboard x (y-3) `thenIO` \(xyN3) ->
    xlookup keyboard x (y-4) `thenIO` \(xyN4) ->
    xlookup keyboard x (y+1) `thenIO` \(xyP1) ->
    xlookup keyboard x (y+2) `thenIO` \(xyP2) ->
    xlookup keyboard x (y+3) `thenIO` \(xyP3) ->
    xlookup keyboard x (y+4) `thenIO` \(xyP4) ->
    xlookup keyboard (x-1) y `thenIO` \(xN1y) ->
    xlookup keyboard (x-2) y `thenIO` \(xN2y) ->
    xlookup keyboard (x-3) y `thenIO` \(xN3y) ->
    xlookup keyboard (x-4) y `thenIO` \(xN4y) ->            
    xlookup keyboard (x+1) y `thenIO` \(xP1y) ->
    xlookup keyboard (x+2) y `thenIO` \(xP2y) ->
    xlookup keyboard (x+3) y `thenIO` \(xP3y) ->
    xlookup keyboard (x+4) y `thenIO` \(xP4y) ->
    xlookup keyboard (x-1) (y-1) `thenIO` \(xN1yN1)->
    xlookup keyboard (x-2) (y-2) `thenIO` \(xN2yN2) ->
    xlookup keyboard (x-3) (y-3) `thenIO` \(xN3yN3) ->
    xlookup keyboard (x-4) (y-4) `thenIO` \(xN4yN4) ->
    xlookup keyboard (x+1) (y+1) `thenIO` \(xP1yP1) ->
    xlookup keyboard (x+2) (y+2) `thenIO` \(xP2yP2) ->
    xlookup keyboard (x+3) (y+3) `thenIO` \(xP3yP3) ->
    xlookup keyboard (x+4) (y+4) `thenIO` \(xP4yP4) ->
    xlookup keyboard (x-1) (y+1) `thenIO` \(xN1yP1) -> 
    xlookup keyboard (x-2) (y+2) `thenIO` \(xN2yP2) ->
    xlookup keyboard (x-3) (y+3) `thenIO` \(xN3yP3) -> 
    xlookup keyboard (x-4) (y+4) `thenIO` \(xN4yP4) -> 
    xlookup keyboard (x+1) (y-1) `thenIO` \(xP1yN1) -> 
    xlookup keyboard (x+2) (y-2) `thenIO` \(xP2yN2) -> 
    xlookup keyboard (x+3) (y-3) `thenIO` \(xP3yN3) -> 
    xlookup keyboard (x+4) (y-4) `thenIO` \(xP4yN4) -> 
    xlookup keyboard (x+1) y `thenIO` \(xP1y) ->
    xlookup keyboard (x+2) y `thenIO` \(xP2y) ->
    xlookup keyboard (x+3) y `thenIO` \(xP3y) ->
    xlookup keyboard (x+4) y `thenIO` \(xP4y) ->
    if ((direct3 xyN1 xyN2 xyN3 xyN4 xy xyP1 xyP2 xyP3 xyP4) +
        (direct3 xN1y xN2y xN3y xN4y xy xP1y xP2y xP3y xP4y) +  
	(direct3 xN1yN1 xN2yN2 xN3yN3 xN4yN4 xy xP1yP1 xP2yP2 xP3yP3 xP4yP4) +
        (direct3 xN1yP1 xN2yP2 xN3yP3 xN4yP4 xy xP1yN1 xP2yN2 xP3yN3 xP4yN4)) 
       >=200 
      then returnIO (True)
      else returnIO (False)