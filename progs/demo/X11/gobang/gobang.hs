module Gobang where

import Xlib
import Utilities
import Redraw
import Weights

getXInfo :: String -> IO XInfo
getXInfo host = 
  xOpenDisplay host `thenIO` \ display ->
  let (screen:_) = xDisplayRoots display 
      fg_pixel = xScreenBlackPixel screen
      bg_pixel = xScreenWhitePixel screen
      root = xScreenRoot screen
  in 
  xCreateWindow root
                (XRect 0 0 900 600)
                [XWinBackground bg_pixel, 
                 XWinEventMask (XEventMask [XButtonPress, 
                                            XKeyPress, 
                                            XExposure])]
                 `thenIO` \ window ->
  xSetWmName window "Gobang" `thenIO` \() ->
  xMapWindow window `thenIO` \() ->
  xOpenFont display "10x20" `thenIO`  \ playerfont ->
  xOpenFont display "6x13" `thenIO` \ genericfont ->
  xCreateGcontext (XDrawWindow window)
                  [XGCBackground bg_pixel,      
                   XGCForeground fg_pixel] `thenIO` \ gcontext  ->
  xCreateGcontext (XDrawWindow window)
                  [XGCBackground fg_pixel,
                   XGCForeground bg_pixel,
                   XGCFont       genericfont] `thenIO` \ gcontext2 ->
  xCreateGcontext (XDrawWindow window)
                  [XGCBackground bg_pixel,
                   XGCForeground fg_pixel,
                   XGCFont       playerfont] `thenIO` \ gcontextp ->
  returnIO (XInfo display window gcontext gcontext2 gcontextp)

demo = main

main = getEnv "DISPLAY" exit $ \ host ->
       xHandleError (\(XError msg) -> appendChan stdout msg exit done) $
       gobang host

gobang :: String -> IO ()
gobang host =
  getXInfo host `thenIO` \ xinfo ->
  xMArrayCreate [1..361] `thenIO` \ board ->
  xMArrayCreate [1..361] `thenIO` \ weight1 ->
  xMArrayCreate [1..361] `thenIO` \ weight2 ->
  xMArrayCreate [1..722] `thenIO` \ steps ->
  xMArrayCreate [""] `thenIO` \ player1 ->
  xMArrayCreate [""] `thenIO` \ player2 ->
  xMArrayCreate [1..4] `thenIO`  \ time ->
  xMArrayCreate [1] `thenIO` \ numbersteps ->
  xMArrayCreate [""] `thenIO` \ promptString ->
  xMArrayCreate [1] `thenIO` \ next_player ->
  let state = GameState player1 player2 board steps weight1 weight2 time
                        numbersteps promptString next_player
  in
  initGame xinfo state `thenIO` \ _ ->
  promptPlayers xinfo state `thenIO` \ _ ->
  playGame xinfo state

promptPlayers xinfo state = 
  let (XInfo display window gcontext gcontext2 gcontextp) = xinfo
      (GameState player1 player2 board steps weight1 weight2 time
                 numbersteps promptString next_player) = state
  in
  promptFor "player 1:" xinfo state `thenIO` \ player1_name ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 710 65) player1_name
  `thenIO` \ _ ->
  xMArrayUpdate player1 0 player1_name `thenIO` \ _ ->
  promptFor "player 2:" xinfo state `thenIO` \ player2_name ->
  xDrawGlyphs (XDrawWindow window) gcontextp (XPoint 710 205) player2_name
  `thenIO` \ _ ->
  xMArrayUpdate player2 0 player2_name `thenIO` \ _ ->
  clearCmd xinfo state

initGame :: XInfo -> GameState -> IO ()
initGame xinfo 
         state@(GameState player1 player2 board steps weight1 weight2 time
                          numbersteps promptString next_player) =
          getTime `thenIO` \ curtime ->
          initArray time 0 2 0 `thenIO` \() ->
          initArray time 2 4 curtime `thenIO` \() ->
          initArray numbersteps 0 1 0 `thenIO` \() ->
          initArray board 0 361 0 `thenIO` \() ->
          initArray weight1 0 361 0 `thenIO` \() ->
          initArray weight2 0 361 0 `thenIO` \ () ->
          initArray next_player 0 1 1 `thenIO` \ () ->
          clearCmd xinfo state `thenIO` \ () ->
          redraw xinfo state
 

handleButton :: XPoint -> XInfo -> GameState -> GameCont -> IO ()
handleButton (XPoint x y) 
             xinfo
             state@(GameState player1 player2 board steps weight1 weight2 time
                              numbersteps promptString next_player)
             cont 
       | buttonPress 700 330 x y  = initArray player1 0 1 "" `thenIO` \ _ ->
                                    initArray player2 0 1 "" `thenIO` \ _ ->
                                    initGame xinfo state `thenIO` \ _ ->
                                    promptPlayers xinfo state `thenIO` \ _ ->
                                    playGame xinfo state
       | buttonPress 700 360 x y  = initGame xinfo state `thenIO` \ _ ->
                                    playGame xinfo state
       | buttonPress 700 390 x y  = undoGame xinfo state cont
       | buttonPress 700 420 x y  = loadGame xinfo state cont
       | buttonPress 700 450 x y  = saveGame xinfo state `thenIO` \ () ->
                                    cont xinfo state
       | buttonPress 700 480 x y  = quitGame xinfo state cont
       | ishelp x y          = helpGame xinfo state `thenIO` \ () ->
                               cont xinfo state
       | otherwise           = cont xinfo state

when :: Bool -> IO () -> IO ()
when cond action = if cond then action else returnIO ()

undoGame xinfo@(XInfo display window gcontext gcontext2 gcontextp)
         state@(GameState player1 player2 board steps weight1 weight2 time
                          numbersteps promptString next_player)
         cont =
  xMArrayLookup next_player 0 `thenIO` \ next_p ->
  xMArrayLookup player1 0 `thenIO` \ name1 ->
  xMArrayLookup player2 0 `thenIO` \ name2 ->
  let undoStep n =
        xMArrayLookup steps (2*n) `thenIO` \ x ->
        xMArrayLookup steps (2*n+1) `thenIO` \ y ->
        xMArrayUpdate board ((x-1)*19 + y-1) 0 `thenIO` \ _ ->
        (if (name1 == "computer" || name2 == "computer") 
            then draw_unit board weight1 weight2 x y 
            else returnIO ()) `thenIO` \ _ ->
       xDrawRectangle (XDrawWindow window) gcontext2 
                      (XRect (x*30-15) (y*30-15) 30 30) True 
       `thenIO` \() ->
--        drawBoard xinfo `thenIO` \ _ ->
--        drawPieces 1 1 board xinfo `thenIO` \ _ ->
        let x30 = x * 30
            y30 = y * 30
            c = XPoint x30 y30
            w = XPoint (x30-15) y30
            e = XPoint (x30+15) y30
            no = XPoint x30 (y30-15)
            s = XPoint x30 (y30+15)
            m = XArc (x30-3) (y30-3) 6 6 (-1.0) 6.283
        in
        when (x > 1) (xDrawLine (XDrawWindow window) gcontext w c) 
        `thenIO` \ _ ->
        when (x < 19) (xDrawLine (XDrawWindow window) gcontext c e) 
        `thenIO` \ _ ->
        when (y > 1) (xDrawLine (XDrawWindow window) gcontext no c) 
        `thenIO` \ _ ->
        when (y < 19) (xDrawLine (XDrawWindow window) gcontext c s) 
        `thenIO` \ _ ->
        when ((x `elem` [4,10,16]) && (y `elem` [4,10,16]))
             (xDrawArc (XDrawWindow window) gcontext m True) 
        `thenIO` \ _ ->
        xDisplayForceOutput display `thenIO` \ _ ->
        xMArrayUpdate numbersteps 0 n `thenIO` \ _ ->
        xMArrayLookup next_player 0 `thenIO` \ next_p ->
        xMArrayUpdate next_player 0 (if next_p == 1 then 2 else 1) 

      cur_name = if next_p == 1 then name1 else name2
      last_name = if next_p == 1 then name2 else name1
  in
  xMArrayLookup numbersteps 0 `thenIO` \ n ->
  if n==0 then drawCmd "No more steps to undo!" xinfo state `thenIO` \ _ ->
               cont xinfo state
  else 
  if cur_name == "computer" then cont xinfo state
  else
  (undoStep (n-1) `thenIO` \_ ->
   if (last_name == "computer" && n /= 1) then undoStep (n-2)
   else
   returnIO ()) `thenIO` \ _ ->
  playGame xinfo state
    



promptFile xinfo state cont =
  promptFor "File name:" xinfo state `thenIO` \ name ->
  readFile name 
           (\ _ -> drawCmd ("Can't read file:" ++ name) xinfo state 
                   `thenIO` \ _ -> 
		   cont XNull)
           (\ content -> cont (XSome content))

loadGame xinfo state cont =
  promptFile xinfo state $ \ file ->
  case file of
    XNull -> cont xinfo state
    XSome file_content ->
     readGameState file_content `thenIO` \ new_state ->
     let (GameState _ _ _ _ _ _ time _ _ _) = new_state
     in
     getTime `thenIO` \ curtime ->
     initArray time 2 4 curtime `thenIO` \() ->
     redraw xinfo new_state `thenIO` \ _ ->
     playGame xinfo new_state

saveGame :: XInfo -> GameState -> IO ()
saveGame xinfo state =
  promptFor "File name:" xinfo state `thenIO` \ name ->
  showGameState state `thenIO` \ str ->
  writeFile name str
            (\ _ -> drawCmd ("Can't write file: " ++ name) xinfo state)
	    done

quitGame :: XInfo -> GameState -> GameCont -> IO ()
quitGame xinfo state cont =
  let (XInfo display window gcontext gcontext2 gcontextp) = xinfo
  in
  promptFor "Are you sure? (y/n)" xinfo state `thenIO` \ reps ->
  if (reps == "y" || reps == "Y") then xCloseDisplay display
                                  else clearCmd xinfo state `thenIO` \ _ ->
                                       cont xinfo state

playGame :: XInfo -> GameState -> IO ()
playGame xinfo state =
     let             
        (XInfo display window gcontext gcontext2 gcontextp) = xinfo
        (GameState player1 player2 board steps weight1 weight2 time
                   numbersteps promptString next_player) = state
     in
     xMArrayLookup numbersteps 0 `thenIO` \ x ->
     (\cont -> if x == 361 
               then drawCmd "It's a tie!" xinfo state `thenIO` \ _ ->
                    let loop xinfo state = waitButton xinfo state (\ _ -> loop)
                    in loop xinfo state
               else cont) $        
     xMArrayLookup next_player 0 `thenIO` \ next_player_num ->
     getTime `thenIO` \ curtime ->
     xMArrayLookup time 0 `thenIO` \ lstm0 ->
     xMArrayLookup time 1 `thenIO` \ lstm1 ->
     xMArrayLookup time 2 `thenIO` \ lstm2 ->
     xMArrayLookup time 3 `thenIO` \ lstm3 ->
     drawCmd ("Waiting for player # " ++ (show next_player_num)) xinfo state 
     `thenIO` \() ->
     if (next_player_num == 1)
        then xDrawGlyph (XDrawWindow window) gcontextp (XPoint 850 70)
                   '<' `thenIO` \(trash) ->
             xDrawRectangle (XDrawWindow window) gcontext2 
	                    (XRect 840 180 40 40) True `thenIO` \() ->
             xMArrayUpdate time 2 curtime `thenIO` \() ->
             xMArrayUpdate time 1 (lstm1+curtime-lstm3) `thenIO` \() ->
             showtime 705 270 (lstm1+curtime-lstm3) xinfo `thenIO` \() ->
             xMArrayLookup player1 0 `thenIO` \ x ->
             if (x == "computer") 
                   then computerplay xinfo state
                   else humanplay xinfo state
        else xDrawGlyph (XDrawWindow window) gcontextp (XPoint 850 210)
                    '<' `thenIO` \(trash) ->
             xDrawRectangle (XDrawWindow window) gcontext2 
	                    (XRect 840 40 40 40)  True `thenIO` \() ->
             xMArrayUpdate time 3 curtime `thenIO` \() ->
             xMArrayUpdate time 0 (lstm0+curtime-lstm2) `thenIO` \() ->
             showtime 705 130 (lstm0+curtime-lstm3) xinfo `thenIO` \() ->
             xMArrayLookup player2 0 `thenIO` \ x ->
             if (x == "computer") 
                   then computerplay xinfo state
                   else humanplay xinfo state

waitButton xinfo@(XInfo display _ _ _ _) state cont = 
  let
    loop xinfo state = 
      xGetEvent display `thenIO` \ event ->
      case (xEventType event) of
        XExposureEvent -> may_redraw (xEventCount event == 0) xinfo state 
                          `thenIO` \ _ ->
                          loop xinfo state
        XButtonPressEvent -> 
                          let pos = xEventPos event
                          in 
                          handleButton pos xinfo state (cont pos)
        _              -> xBell display 0 `thenIO` \ _ ->
                          loop xinfo state
  in
  loop xinfo state

updateboard :: XInfo -> GameState -> Int -> Int -> IO ()
updateboard xinfo state x y = 
            let (GameState player1 player2 board steps weight1 weight2 time
                           numbersteps promptString next_player) = state
                (XInfo display window gcontext gcontext2 gcontextp) = xinfo
            in
            xMArrayLookup next_player 0 `thenIO` \ next_player_num ->
            xMArrayUpdate next_player 0 (if next_player_num == 1 then 2 else 1)
            `thenIO` \ _ -> 
            xMArrayLookup numbersteps 0 `thenIO` \ z ->
            xMArrayUpdate numbersteps 0 (z+1) `thenIO` \() ->
            xMArrayUpdate steps (2*z) x `thenIO` \() ->
            xMArrayUpdate steps (2*z+1) y `thenIO` \() ->
            xMArrayLookup player1 0 `thenIO` \ name1 ->
            xMArrayLookup player2 0 `thenIO` \ name2 ->
            xMArrayUpdate board (19*(x-1)+y-1) next_player_num 
            `thenIO` \() ->
            human_unit board x y `thenIO` \ win ->
            if win 
            then drawCmd ("Player " ++ (show next_player_num) ++ " has won!")
                         xinfo state `thenIO` \ _ ->
                 let loop xinfo state = waitButton xinfo state (\ _ -> loop)
                 in loop xinfo state
            else if (name1 == "computer" || name2 == "computer")
                 then draw_unit board weight1 weight2 x y `thenIO` \() ->
                      xMArrayUpdate weight1 (19*(x-1)+y-1) (-1) `thenIO` \() ->
                      xMArrayUpdate weight2 (19*(x-1)+y-1) (-1) `thenIO` \() ->
                      playGame xinfo state
                 else playGame xinfo state

choice :: XPoint -> XInfo -> GameState -> IO ()
choice (XPoint x y) xinfo@(XInfo display _ _ _ _) state =
   let (GameState player1 player2 board steps weight1 weight2 time
                  numbersteps promptString next_player) = state
   in
   case (getposition x y) of
     XNull -> humanplay xinfo state
     XSome (x, y) -> 
       xMArrayLookup board (19*(x-1)+y-1) `thenIO` \ z ->
       if (z>0)
       then xBell display 0 `thenIO` \ _ ->
            drawCmd "Wrong point, please re-enter" xinfo state `thenIO` \() ->
            humanplay xinfo state
       else xMArrayLookup next_player 0 `thenIO` \ next_player_num ->
            drawPiece x y xinfo (next_player_num == 1) `thenIO` \() ->
            updateboard xinfo state x y

humanplay :: XInfo -> GameState -> IO ()
humanplay xinfo state =  waitButton xinfo state choice

computerplay :: XInfo -> GameState -> IO ()
computerplay xinfo@(XInfo display window gcontext gcontext2 gcontextp)
             state = 
    let process_events xinfo state cont =
          xEventListen display `thenIO` \ n_event ->
          if n_event == 0 then cont xinfo state
          else xGetEvent display `thenIO` \ event ->
               case (xEventType event) of
                 XButtonPressEvent -> 
                            handleButton (xEventPos event) xinfo state cont
                 XExposureEvent    -> 
                            may_redraw (xEventCount event == 0)
                                       xinfo state 
                            `thenIO` \ _ ->
                            process_events xinfo state cont
                 XKeyPressEvent    ->
                            process_events xinfo state cont
    in
    process_events xinfo state $ 
    \ xinfo@(XInfo display window gcontext gcontext2 gcontextp)              
      state@(GameState _ _ _ _ weight1 weight2 _ numbersteps _ next_player) ->
    robot numbersteps weight1 weight2 `thenIO` \pt ->
    let (XPoint x y) = pt
    in 
    xMArrayLookup next_player 0 `thenIO` \ next_player_num ->
    drawPiece x y xinfo (next_player_num == 1) `thenIO` \() ->
    updateboard xinfo state x y




