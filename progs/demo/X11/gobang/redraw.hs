module Redraw where

import Xlib 
import Utilities

may_redraw :: Bool -> XInfo -> GameState -> IO ()
may_redraw ok xinfo state = if ok then redraw xinfo state else returnIO ()

redraw :: XInfo -> GameState -> IO ()

redraw xinfo state = 
  let (XInfo display window gcontext gcontext2 gcontextp) = xinfo
  in
  xDrawRectangle (XDrawWindow window) gcontext2 (XRect 0 0 900 600) True 
  `thenIO` \ _ ->
  drawBoard xinfo `thenIO` \ () ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 610 65) "Player 1" 
  `thenIO` \ _  ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 620 125) "Clock 1"
  `thenIO` \ _  ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 610 205) "Player 2"
  `thenIO` \ _  ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 620 265) "Clock 2"
  `thenIO` \ _  ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 700 45 130 30) False 
  `thenIO` \ () ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 700 105 90 30) False
  `thenIO` \ () ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 700 185 130 30) False
  `thenIO` \() ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 700 245 90 30) False 
  `thenIO` \() ->
  button 700 330 "New players"  xinfo `thenIO` \() ->
  button 700 360 "New game"  xinfo `thenIO` \() ->
  button 700 390 "Undo" xinfo `thenIO` \() ->
  button 700 420 "Load" xinfo `thenIO` \() ->
  button 700 450 "Save"  xinfo `thenIO` \() ->
  button 700 480 "Quit" xinfo `thenIO` \() ->
  helpButton xinfo `thenIO` \ _ ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 615 535 250 30) False
  `thenIO` \ _ ->
  let (GameState player1 player2 board steps weight1 weight2 time
                 numbersteps promptString next_player) = state
  in
  xMArrayLookup time 0 `thenIO` \ lstm0 ->
  xMArrayLookup time 1 `thenIO` \ lstm1 ->
  showtime 705 270 (lstm1) xinfo `thenIO` \() ->
  showtime 705 130 (lstm0) xinfo `thenIO` \() ->
  xMArrayLookup player1 0 `thenIO` \ player1_name ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 710 65) player1_name
  `thenIO` \ _ ->
  xMArrayLookup player2 0 `thenIO` \ player2_name ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 710 205) player2_name
  `thenIO` \ _ ->
  xMArrayLookup promptString 0 `thenIO` \ ps ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 620 550) ps
  `thenIO` \ _ ->
  xMArrayLookup next_player 0 `thenIO` \ next_player_num ->
  (if (next_player_num == 1)
   then xDrawGlyph (XDrawWindow window) gcontextp (XPoint 850 70) '<' 
   else xDrawGlyph (XDrawWindow window) gcontextp (XPoint 850 210) '<')
  `thenIO` \ _ ->
  drawPieces 1 1 board xinfo `thenIO` \ _ ->
  returnIO ()  

drawHelp (XInfo display window gcontext gcontext2 gcontextp) = 
  xDrawRectangle (XDrawWindow window) gcontext2 (XRect 100 100 300 200) True
  `thenIO` \ _ ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 100 100 300 200) False
  `thenIO` \ _ ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 102 102 296 196) False
  `thenIO` \ _ ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 200 230 100 60) False
  `thenIO` \ _ ->
  xDrawRectangle (XDrawWindow window) gcontext (XRect 202 232 96 56) False
  `thenIO` \ _ ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 240 265) "OK"
  `thenIO` \ _ ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 120)
              "Two players in turn place black and white"
  `thenIO` \ _ ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 135)
              "pieces on the board. The winner is the"
  `thenIO` \ _ ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 150)
              "player who first makes five consecutive"
  `thenIO` \ _ ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 165)
              "pieces in either vertical, horizontal or"
  `thenIO` \ _ ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 180)
              "diagonal directions."
  `thenIO` \ _ ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 200)
              "To play with a robot, type \"computer\" as"
  `thenIO` \ _ ->
  xDrawGlyphs (XDrawWindow window) gcontext (XPoint 120 215)
              "the name of another player."


drawBoard (XInfo display window gcontext gcontext2 gcontextp) =
  drawvlines 30 30 1 `thenIO` \() ->
  drawhlines 30 30 1 `thenIO` \() ->  
  drawmarks where

  drawvlines :: Int -> Int -> Int -> IO ()
  drawvlines x y z 
                | z <= 19 
                   = xDrawLine (XDrawWindow window) gcontext
                     (XPoint x y) (XPoint x (y+30*18)) `thenIO` \() ->  
		       drawvlines (x+30) y (z+1)
                | otherwise
                   = returnIO ()

  drawhlines :: Int -> Int -> Int -> IO ()
  drawhlines x y z 
                | z <= 19
                   = xDrawLine (XDrawWindow window) gcontext
                     (XPoint x y) (XPoint (x+30*18) y) `thenIO` \() -> 
                       drawhlines x (y+30) (z+1)
                | otherwise 
                   = returnIO ()

  drawmarks :: IO ()
  drawmarks =
            map2IO (\x y ->
                     xDrawArc (XDrawWindow window) gcontext 
                              (XArc x y 6 6 (-1.0) 6.283) True)
                   (map (\x -> 30 + x*30-3) [3,9,15,3,9,15,3,9,15])
                   (map (\x -> 30 + x*30-3) [3,3,3,9,9,9,15,15,15])
            `thenIO` \ _ -> returnIO ()

map2IO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]

map2IO f [] []         = returnIO []
map2IO f (x:xs) (z:zs) = f x z `thenIO` \ y -> 
		         map2IO f xs zs `thenIO` \ ys -> 
		         returnIO (y:ys)

drawPieces 20 _ board xinfo = returnIO ()
drawPieces x 20 board xinfo = drawPieces (x+1) 1 board xinfo
drawPieces x y board xinfo = 
  xMArrayLookup board ((x-1)*19 + y-1) `thenIO` \ piece ->
  (if (piece == 1 || piece == 2)
   then drawPiece x y xinfo (piece == 1)
   else returnIO ()) `thenIO` \ _ ->
  drawPieces x (y+1) board xinfo
  
drawPiece x y (XInfo display window gcontext gcontext2 _ ) is_black =
  (if is_black then returnIO ()
               else xDrawArc (XDrawWindow window) gcontext2 
                             (XArc (30*x-10) (30*y-10) 20 20
                             (-1.0) 6.283)
                             True) `thenIO` \ _ -> 
  xDrawArc (XDrawWindow window) gcontext 
           (XArc (30*x-10) (30*y-10) 20 20
  	   (-1.0) 6.283)
           is_black `thenIO` \ _ ->
  xDisplayForceOutput display

