module R_Display (displaym) where

import R_Ptypes
import R_Utility
import Xlib
import R_Constants

displaym :: String -> Int -> Movie -> IO ()

displaym host n movie =
  let
    movie' = cycle (take n (map (map translatePoly) movie))
  in
  xOpenDisplay host 
  `thenIO` \ display ->
  let (screen:_) = xDisplayRoots display
      fg_color = xScreenBlackPixel screen
      bg_color = xScreenWhitePixel screen
      color_map = xScreenDefaultColormap screen
      getPixels [] = returnIO []
      getPixels (c:cs) = 
        xLookupColor color_map c `thenIO` \ (xc, _) ->
     	xAllocColor color_map xc `thenIO` \ (p,_,_) ->
        getPixels cs `thenIO` \ ps ->
        returnIO (p:ps) 
  in
  getPixels (map colorName allColors) 
  `thenIO` \ pixels ->
  let
    lookupPixel c = lookupPixel1 c allColors pixels

    lookupPixel1 x []     _      = head pixels
    lookupPixel1 x (c:cs) (p:ps) = 
      if x == c then p
                else lookupPixel1  x cs ps
    parent = xScreenRoot screen
  in
  xMArrayCreate [lookupPixel i | i <- [0..15]] 
  `thenIO` \ pixelArray ->
  xCreateGcontext (XDrawWindow parent)
                  [XGCBackground bg_color,
                   XGCForeground fg_color]
  `thenIO` \ gcontext ->
  xCreateGcontext (XDrawWindow parent)
                  [XGCBackground bg_color,
                   XGCForeground bg_color] 
  `thenIO` \ blank_gcontext ->
  xCreateWindow parent
                (XRect 100 100 500 500)
                [XWinBackground bg_color,
                 XWinEventMask (XEventMask [XButtonPress])] 
  `thenIO` \window ->
  let depth = xDrawableDepth (XDrawWindow window) 
  in
  xCreatePixmap (XSize 500 500) depth (XDrawWindow parent)
  `thenIO` \ pixmap ->
  xMapWindow window 
  `thenIO` \() ->
  let
    dispFrame m = 
      xDrawRectangle (XDrawPixmap pixmap) 
                     blank_gcontext 
		     (XRect 0 0 500 500) 
		     True 
      `thenIO_`
      dispPic m 
      `thenIO_`
      xCopyArea (XDrawPixmap pixmap) gcontext (XRect 0 0 500 500) 
                (XDrawWindow window) (XPoint 0 0) 
      `thenIO_`
      xDisplayForceOutput display

    dispPic [] = returnIO ()
    dispPic (p:ps) = dispPoly p `thenIO_` dispPic ps

    dispPoly (c, vec) =
--      xLookupColor color_map (colorName c) `thenIO` \ ec ->
--      xAllocColor color_map ec `thenIO` \ p -> 
      xMArrayLookup pixelArray c `thenIO` \p ->
      xUpdateGcontext gcontext [XGCForeground p] `thenIO` \ () ->
--      xSetGcontextForeground gcontext (lookupPixel c) `thenIO` \ () ->
      xDrawLines (XDrawPixmap pixmap) gcontext vec True

    untilButton3 (frame:frames) = 
      let 
        action = dispFrame frame `thenIO_` untilButton3 frames
      in
      xEventListen display `thenIO` \count ->
      if count == 0 then action else
      xGetEvent display `thenIO` \event ->
        case (xEventType event) of
	 XButtonPressEvent -> 
	   case (xEventCode event) of
	     3 -> returnIO ()
	     _ -> action
         _                       -> action
  in
  printString ("Click right button to end.\n") `thenIO_`
  untilButton3 movie' `thenIO_`
  xFreePixmap pixmap `thenIO_`
  xCloseDisplay display

type Movie' = [Pic']
type Pic' = [Poly']
type Poly' = (Int, [XPoint])

translatePoly :: Poly -> Poly'
translatePoly (c, vs) = (c, flatten_2 vs)

flatten_2 []        = []
flatten_2 ((a,b):r) = (XPoint (a `div` 2) (b `div` 2)):(flatten_2 r)

printString :: String -> IO ()
printString s = appendChan "stdout" s abort (returnIO ())
