module Draw where

import Xlib 

main = getEnv "DISPLAY" exit (\ host -> draw host)

draw :: String -> IO ()
draw host =
  xOpenDisplay host `thenIO` \ display ->
  let (screen:_) = xDisplayRoots display
      fg_color = xScreenBlackPixel screen
      bg_color = xScreenWhitePixel screen
      root = xScreenRoot screen
  in
  xCreateWindow root
                (XRect 100 100 400 400)
                [XWinBackground bg_color,
                 XWinEventMask (XEventMask [XButtonMotion, 
		                            XButtonPress,
                                            XKeyPress])] 
  `thenIO` \window ->
  xMapWindow window `thenIO` \() ->
  xCreateGcontext (XDrawWindow root)
                  [XGCBackground bg_color,
                   XGCForeground fg_color] `thenIO` \ gcontext ->
  let
    handleEvent :: XPoint -> IO ()
    handleEvent last =
      xGetEvent display `thenIO` \event ->
        let pos = xEventPos event
	in        
	case (xEventType event) of
          XButtonPressEvent  -> handleEvent pos
          XMotionNotifyEvent -> 
            xDrawLine (XDrawWindow window) gcontext last pos `thenIO` \() ->
	    handleEvent pos
          XKeyPressEvent     -> xCloseDisplay display
          _                  -> handleEvent last
  in
  appendChan stdout "Press any key to quit.\n" exit done `thenIO` \ _ ->
  handleEvent (XPoint 0 0)
