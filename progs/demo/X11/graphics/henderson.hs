-- Peter Henderson's Recursive Geometry
-- Syam Gadde and Bo Whong
-- full set of modules
-- CS429 Project
-- 4/30/93

module HendersonLib (Hostname(..), Filename(..), VTriple(..), HendQuartet(..),
                     Picture(..), sendToDraw, draw, create, modify, plot) where
import Xlib

-- ADTs and Type Synonyms --------------------------------------------------
data Picture = Nil
             | Flip Picture
             | Beside Float Picture Float Picture
	     | Above Float Picture Float Picture
	     | Rot Picture
	     | File String
	     | Overlay Picture Picture
	     | Grid Int Int SegList
	     deriving Text

data Plot = Plot Picture VTriple
          | Union Plot Plot

type Hostname = String
type Filename = String
type IntPoint = (Int,Int)
type IntSegment = (IntPoint, IntPoint)
type IntSegList = [IntSegment]
type Point = (Float,Float)
type Segment = (Point, Point)
type SegList = [Segment]
type Vector = Point
type VTriple = (Vector, Vector, Vector)
type HendQuartet = (Int, Int, Int, Int)
type PEnv = [(Filename, Picture)]

-- vector Functions --------------------------------------------------------
-- for adding, negating, multiplying, and dividing vectors

addV :: Vector -> Vector -> Vector
addV (x1,y1) (x2,y2) = (x1+x2, y1+y2)

negateV :: Vector -> Vector
negateV (x,y) = (-x,-y)

multV ::  Float-> Vector -> Vector
multV a (x,y) = (a*x, a*y)

divV :: Float -> Vector -> Vector
divV a (x,y) = (x/a, y/a)

-- plot Function -----------------------------------------------------------
-- picture manipulation function

plot :: Picture -> VTriple -> PEnv -> ((Plot, PEnv) -> IO()) -> IO()

-- the Nil Picture is just "nothingness" so choose an abritrary representation
--  of nothingness.
plot Nil (v1, v2, v3) env cont = 
  plot (Grid 1 1 []) (v1,v2,v3) env cont

-- Flipping a Picture
plot (Flip p1) (v1, v2, v3) env cont = 
  plot p1 (addV v1 v2, negateV v2, v3) env cont

-- Rotate a Picture 90 degrees counterclockwise
plot (Rot p1) (v1, v2, v3) env cont = 
  plot p1 (addV v1 v3, negateV v3, v2) env cont

-- Overlay one Picture over another Picture
plot (Overlay p q) (a,b,c) env cont =
  plot p (a,b,c) env $ \ (plot1, env1) ->
  plot q (a,b,c) env1 $ \ (plot2, env2) ->
  cont ((Union plot1 plot2), env2)

-- Place p1 Beside p2 with width ratio m to n
plot (Beside m p1 n p2) (v1, v2, v3) env cont = 
  plot p1 (v1, multV (m/(m+n)) v2, v3) env $ \ (plot1, env1) ->
  plot p2 ((addV (multV (m/(m+n)) v2) v1), 
	         (multV (n/(m+n)) v2), 
                 v3) env1                  $ \ (plot2, env2) ->
  cont ((Union plot1 plot2), env2)

-- Place p Above q with height ratio m to n
plot (Above m p n q) (a,b,c) env cont =
  plot q (addV a (multV (m/(n+m)) c), b,  multV (n/(m+n)) c) env 
    $ \ (plot1, env1) ->
  plot p (a, b, multV (m/(m+n)) c) env1 $ \ (plot2, env2) ->
  cont ((Union plot1 plot2), env2)

-- the 'real' Picture
plot (Grid x y s) (a,b,c) env cont =
  cont ((Plot (Grid x y s) (a,b,c)), env)

-- this picture is located in a File with name name
--  lookup table: thanks to Sheng
plot (File name) (a,b,c) env cont =
  case (lookupEnv env name) of
    ((_, pic):_) -> plot pic (a,b,c) env cont
    []           ->
       readFile name (\s -> appendChan stdout ("File "++name++" not able to be read\n") exit done)
                     $ \s ->
       let 
        pic = read s 
        newenv = (name,pic):env
       in
       plot pic (a,b,c) newenv cont 

lookupEnv :: PEnv -> Filename -> PEnv
lookupEnv [] _ = []
lookupEnv ((a,b):es) name | a==name   = ((a,b):es)
                          | otherwise = lookupEnv es name

-- Draw Function -----------------------------------------------------------
-- user function to draw pictures 

draw :: Hostname -> Picture -> VTriple -> HendQuartet -> IO()

-- opens a display, screen, and window (of size specified in HendQuartet)
--  and draws Picture in the window
draw host p (a,b,c) (hm,hn,ho,hp) = 
 xOpenDisplay host `thenIO` \display ->       -- opens display
  let (screen:_) = xDisplayRoots display
      fg_color = xScreenBlackPixel screen
      bg_color = xScreenWhitePixel screen
      root = xScreenRoot screen
  in 
  xCreateWindow root                          -- opens window
                (XRect hm hn ho hp)
		[XWinBackground bg_color,
		 XWinEventMask (XEventMask [XKeyPress, 
		                            XExposure, 
                                            XButtonPress])]
  `thenIO` \window ->
  xSetWmName window "Henderson Graphics" `thenIO` \() ->
  xSetWmIconName window "Henderson Graphics" `thenIO` \() ->
  xMapWindow window `thenIO` \() ->          -- show window
  xDisplayForceOutput display `thenIO` \ () ->  -- show window NOW
  xCreateGcontext (XDrawWindow (xScreenRoot screen))   -- open a GC
                  [XGCBackground bg_color,
		   XGCForeground fg_color] `thenIO` \ gcontext ->
  plot p (a,b,c) [] $ \(plt,_) ->            -- make pic easier to work with
  let
    handleEvent =
      xGetEvent display `thenIO` \event ->
        case (xEventType event) of
	  -- Has a part of the window been uncovered?
	  XExposureEvent ->  sendToDraw window screen display gcontext plt
	                     `thenIO` \() -> handleEvent
          _              -> xCloseDisplay display
  in
  handleEvent

-- SendToDraw Function -----------------------------------------------------
-- called by draw to actually draw the lines onto the window

sendToDraw :: XWindow -> XScreen -> XDisplay -> XGcontext -> Plot -> IO()

-- have a Union.  so do one, and then the other. simple.
sendToDraw win screen display gcontext (Union p1 p2) = 
  sendToDraw win screen display gcontext p1 `thenIO` \() ->
  sendToDraw win screen display gcontext p2

-- have just a Plot.  have to do some dirty work.
sendToDraw window screen display gcontext (Plot (Grid x y s) (a,b,c)) = 
  let 
    v2p :: Vector -> XPoint
    v2p (e,f) = XPoint (round e) (round f)  -- convert Vector to an XPoint
    fx :: Float
    fx = fromIntegral x
    fy :: Float
    fy = fromIntegral y
    drawit :: SegList -> IO()
    -- draw the Grid one line at a time
    drawit [] = done
    drawit (((x0,y0),(x1,y1)):ss) =
      xDrawLine (XDrawWindow window) 
              gcontext 
	      (v2p (addV (addV a (multV (x0/fx) b))
	                 (multV (y0/fy) c)))
	      (v2p (addV (addV a (multV (x1/fx) b))
	                 (multV (y1/fy) c))) `thenIO` \() ->
      drawit ss
  in
  drawit s `thenIO` \ () ->
  xDisplayForceOutput display

-- create function ---------------------------------------------------------
-- opens up a window to allow the user to create a file 
-- and save it onto a file

create :: Hostname -> Filename -> Int -> Int -> IO()

create host filename x y =
  xOpenDisplay host `thenIO` \ display ->
  let 
   (screen:_) = xDisplayRoots display
   fg_color = xScreenWhitePixel screen
   bg_color = xScreenBlackPixel screen
   root = xScreenRoot screen
  in
  xCreateWindow root
                (XRect 0 0 (x+1) (y+1))
                [XWinBackground bg_color,
                 XWinEventMask (XEventMask [XExposure,
		                            XKeyPress, 
					    XButtonPress,
					    XPointerMotion])]
  `thenIO` \window ->
  xSetWmName window filename `thenIO` \() ->
  xSetWmIconName window filename `thenIO` \() ->
  xCreateWindow root
                (XRect 0 0 100 40)
		[XWinBackground bg_color] `thenIO` \window2 ->
  xSetWmName window2 "pos" `thenIO` \() ->
  xSetWmIconName window2 "pos" `thenIO` \() ->
  xMapWindow window `thenIO` \() ->
  xMapWindow window2 `thenIO` \() ->
  xListFonts display "*times*bold*r*normal*18*" `thenIO` \fontlist ->
  xCreateGcontext (XDrawWindow root)
                  [XGCBackground bg_color,
                   XGCForeground fg_color,
		   XGCFont (head fontlist)] `thenIO` \gcontext ->
  let
   handleEvent :: IntSegList -> IO()
   handleEvent list =
     xGetEvent display `thenIO` \event ->
     let 
      point = xEventPos event 
      XPoint pointx pointy = point
      handleEvent' :: XPoint -> IO()
      handleEvent' last = 
       xGetEvent display `thenIO` \event2 ->
       let 
        pos = xEventPos event2
	XPoint posx posy = pos 
       in
        case (xEventType event2) of
         XKeyPressEvent  -> 
           appendChan stdout ((show (tup pos))++ "\n") abort $
           xDrawLine (XDrawWindow window) gcontext point pos 
           `thenIO` \() -> handleEvent (store list point pos)
         XExposureEvent  -> 
           redraw window gcontext list `thenIO` \() -> handleEvent' last
	 XMotionNotifyEvent ->
	   xDrawImageGlyphs (XDrawWindow window2)
	                    gcontext
			    (XPoint 2 18)
			    ((show posx)++", "++(show posy)++"      ") 
			    `thenIO` \dummy -> handleEvent' last
         _                  -> 
           handleEvent' last
     in 
     case (xEventType event) of 
       XButtonPressEvent     -> 
         putFile display filename list x y "create"
       XKeyPressEvent  -> 
         appendChan stdout (show (tup point)) abort $ 
         handleEvent' point 
       XExposureEvent  -> 
         redraw window gcontext list `thenIO` \() -> handleEvent list
       XMotionNotifyEvent ->
	 xDrawImageGlyphs (XDrawWindow window2)
	                  gcontext
			  (XPoint 2 18)
			  ((show pointx)++", "++(show pointy)++"      ") 
			  `thenIO` \dummy -> handleEvent list
       _                  -> 
         handleEvent list
  in 
   case (checkFile filename) of 
     True  -> handleEvent []
     False -> appendChan stdout picTypeError abort $
              xCloseDisplay display

-- modify function ---------------------------------------------------------
-- allows the user to add onto an already existing picture file

modify :: Hostname -> Filename -> IO()

modify host filename =
  case (checkFile filename) of 
   False -> appendChan stdout picTypeError abort done
   True  -> 
    readFile filename (\s -> appendChan stdout 
                                        readError abort done) $ \s->
    let 
     dat = read s 
     origlist = fFloat (getlist dat)
     x = getx dat
     y = gety dat
    in
     xOpenDisplay host `thenIO` \ display ->
     let 
      (screen:_) = xDisplayRoots display
      fg_color = xScreenWhitePixel screen
      bg_color = xScreenBlackPixel screen
      root = xScreenRoot screen
     in
     xCreateWindow root
       (XRect 0 0 (x + 1) (y + 1))
        [XWinBackground bg_color,
        XWinEventMask (XEventMask [XExposure, XKeyPress, 
                                   XButtonPress, XPointerMotion])]
     `thenIO` \window ->
     xSetWmName window filename `thenIO` \() ->
     xSetWmIconName window filename `thenIO` \() ->
     xCreateWindow root (XRect 0 0 100 40)
	[XWinBackground bg_color] `thenIO` \window2 ->
     xSetWmName window2 "pos" `thenIO` \() ->
     xSetWmIconName window2 "pos" `thenIO` \() ->
     xMapWindow window `thenIO` \() -> 
     xMapWindow window2 `thenIO` \() ->
     xListFonts display "*times*bold*r*normal*18*" `thenIO` \fontlist ->
     xCreateGcontext (XDrawWindow root) [XGCBackground bg_color, 
                                         XGCForeground fg_color, 
                                         XGCFont (head fontlist)] 
     `thenIO` \ gcontext ->
    let
     handleEvent :: IntSegList -> IO()
     handleEvent list =
      xGetEvent display `thenIO` \event ->
      let 
       point = xEventPos event 
       XPoint pointx pointy = point
       handleEvent' :: XPoint -> IO()
       handleEvent' last = xGetEvent display `thenIO` \event2 ->
        let 
         pos = xEventPos event2
	 XPoint posx posy = pos 
        in
         case (xEventType event2) of
          XExposureEvent  -> 
            redraw window gcontext list `thenIO` \() -> 
            handleEvent' last
          XKeyPressEvent  -> 
            appendChan stdout ((show (tup pos))++ "\n") abort $
            xDrawLine (XDrawWindow window) gcontext point pos 
            `thenIO` \() -> handleEvent (store list point pos)
     	  XMotionNotifyEvent ->
	    xDrawImageGlyphs (XDrawWindow window2) gcontext 
             (XPoint 2 18) ((show posx)++", "++(show posy)++"      ") 
	    `thenIO` \dummy -> handleEvent' last
	  _                  -> handleEvent' last
      in
       case (xEventType event) of 
        XButtonPressEvent  ->
          putFile display filename list x y "modify"
        XKeyPressEvent     ->
          appendChan stdout (show (tup point)) abort $ 
          handleEvent' point 
        XExposureEvent  -> 
          redraw window gcontext list `thenIO` \() -> 
          handleEvent list
        XMotionNotifyEvent ->
          xDrawImageGlyphs (XDrawWindow window2) 
                           gcontext (XPoint 2 18)
           ((show pointx)++", "++(show pointy)++"      ")
          `thenIO` \dummy -> handleEvent list
        _                  -> 
          handleEvent list
    in
     redraw window gcontext origlist `thenIO` \() -> 
      handleEvent origlist

-- Miscellaneous functions -------------------------------------------------
-- shared by the create and modify functions

checkFile :: Filename -> Bool
checkFile name =
  case (take 4 (reverse name)) of
   "cip." -> True
   _      -> False

store :: IntSegList -> XPoint -> XPoint -> IntSegList 
store l a b =  [((xof a,yof a),(xof b,yof b))] ++ l

xof :: XPoint -> Int
xof (XPoint x y) = x

yof :: XPoint -> Int
yof (XPoint x y) = y

tup :: XPoint -> IntPoint
tup (XPoint a b) = (a,b)
  
ll:: IntSegment -> Int
ll ((a1,a2),(b1,b2)) = a1

lr:: IntSegment -> Int
lr ((a1,a2),(b1,b2)) = a2

rl:: IntSegment -> Int
rl ((a1,a2),(b1,b2)) = b1

rr:: IntSegment -> Int
rr ((a1,a2),(b1,b2)) = b2

getx :: Picture -> Int
getx (Grid m n o) = m

gety :: Picture -> Int
gety(Grid m n o) = n

getlist :: Picture -> SegList
getlist (Grid m n o) = o

fFloat :: SegList -> IntSegList
fFloat = map (\ ((ix,iy),(jx,jy)) ->
             ((round ix,round iy), (round jx,round jy)))

readError :: String
readError  = "Error: reading an invalid file\n"

picTypeError :: String
picTypeError = "Error: files need to be of .pic type\n"

deleteError :: String
deleteError = "Error: file can not be deleted\n"

writeError :: String
writeError = "Error: file can not be written\n"

modError :: String
modError = "Error: file can not be modified\n"

redraw :: XWindow-> XGcontext -> IntSegList -> IO()
redraw window gcontext [] = done
redraw window gcontext (l:ls) = 
 xDrawLine (XDrawWindow window) gcontext (XPoint (ll l) (lr l)) 
                                         (XPoint (rl l) (rr l))
 `thenIO` \() -> redraw window gcontext ls

changeList :: IntSegList -> SegList
changeList = 
  map (\ ((ix,iy),(jx,jy)) -> ((fromIntegral ix,fromIntegral iy),
                               (fromIntegral jx,fromIntegral jy)))

putFile :: XDisplay -> Filename -> IntSegList -> 
           Int -> Int -> String -> IO()
putFile display name list x y flag = 
 let  
  text = show (Grid x y (changeList list))
  finishMsg  = name ++ ": Done...Process completed\n"
  modMsg = name ++ ": Modifying file\n"
  createMsg = name ++ ": Creating file\n"
  continue = 
   deleteFile name (\s -> appendChan stdout deleteError abort done) $
   writeFile name text (\s -> appendChan stdout writeError abort done) $
   appendChan stdout finishMsg abort $ 
   xCloseDisplay display
 in 
  case (flag == "create") of
   False -> appendChan stdout modMsg 
                       (\s -> appendChan stdout modError abort done) $
            continue
   True  -> readFile name (\s -> appendChan stdout createMsg abort $
                                 writeFile name text abort 
                                   (xCloseDisplay display)) $ \s ->
            continue




