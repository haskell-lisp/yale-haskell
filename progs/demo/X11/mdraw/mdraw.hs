module MDraw where

import Xlib 

mapIO :: (a -> IO b) -> [a] -> IO [b]

mapIO f []     = returnIO []
mapIO f (x:xs) = f x `thenIO` \ y -> 
                 mapIO f xs `thenIO` \ ys -> 
		 returnIO (y:ys)

map2IO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]

map2IO f [] []         = returnIO []
map2IO f (x:xs) (z:zs) = f x z `thenIO` \ y -> 
		         map2IO f xs zs `thenIO` \ ys -> 
		         returnIO (y:ys)

xGetEventMul              :: XMArray XDisplay -> IO (Int, XEvent)
xGetEventMul displays = 
  let n_displays = xMArrayLength displays
      loop :: Int -> IO (Int, XEvent)
      loop i = if i == n_displays then loop 0
               else xMArrayLookup displays i `thenIO` \ display ->
                    xDisplayForceOutput display `thenIO` \ _ ->
                    xEventListen display `thenIO` \ n_events ->
                    if n_events == 0 then loop (i + 1)
                    else xGetEvent display `thenIO` \ event ->
                         returnIO (i, event)
  in loop 0

-- takes a list of host names

mdraw :: [String] -> IO ()
mdraw hosts =
  xHandleError (\ (XError msg) -> appendChan stdout msg exit done) $
  mapIO xOpenDisplay hosts `thenIO` \ displays ->
  let screens = map (head . xDisplayRoots) displays
      fg_colors = map xScreenBlackPixel screens
      bg_colors = map xScreenWhitePixel screens
      roots = map xScreenRoot screens
  in
  map2IO (\ root color -> 
              xCreateWindow root 
                            (XRect 100 100 400 400)
                            [XWinBackground color,
		             XWinEventMask (XEventMask [XButtonMotion, 
                                                        XButtonPress])])
         roots
         bg_colors 
  `thenIO` \windows ->
  mapIO xMapWindow windows `thenIO` \ _ ->
  map2IO xCreateGcontext 
        (map XDrawWindow roots) 
        (map (\ color -> [XGCForeground color]) fg_colors)
  `thenIO` \ gcontexts ->
  xMArrayCreate displays `thenIO` \ displayArr ->
  let
    handleEvent lasts =
      xGetEventMul displayArr `thenIO` \ (idx, event) ->
        let pos = xEventPos event
	in
	case (xEventType event) of
          XButtonPressEvent  -> 
            xMArrayUpdate lasts idx pos `thenIO` \ () ->
            handleEvent lasts
          XMotionNotifyEvent ->
            xMArrayLookup lasts idx `thenIO` \ last -> 
            map2IO (\ window gcontext -> xDrawLine (XDrawWindow window) 
                                                    gcontext 
                                                    last 
                                                    pos)
                   windows
                   gcontexts
            `thenIO` \ _ ->
            xMArrayUpdate lasts idx pos `thenIO` \ () ->
            handleEvent lasts
          _                  -> handleEvent lasts
  in
  xMArrayCreate (map (\ _ -> XPoint 0 0) hosts) `thenIO` \ lasts ->
  handleEvent lasts `thenIO` \ _ ->
  returnIO ()

