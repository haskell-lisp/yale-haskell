module Xlib(XLibTypes..,XLibPrims..) where
import XLibTypes
import XLibPrims

module XLibTypes(XDisplay, XScreen, XWindow, XGcontext, XPixmap,
                 XColormap, XCursor, XFont, XImage, XMaybe(..), XError(..),
                 XBitmap(..), XKeysymTable(..), XBitVec(..),
                 XPixarray(..), XByteVec(..), XAtom(..), XProperty(..),
                 XPixel(..), XDrawable(..), XTime(..), XSwitch(..),
		 XWindowPlace(..), XEventMode(..), XEventKind(..),
		 XWindowVisibility(..), XWindowStackMode(..),
		 XPropertyState(..), XMapReqType(..), XGraphFun(..),
		 XEvent(..), XEventType(..), XEventSlot(..), XEventMask(..),
		 XEventMaskKey(..), XStateMask(..), XStateMaskKey(..),
		 XWinAttribute(..),XGCAttribute(..), XImAttribute(..), 
		 XGrabAttribute(..), XArcMode(..), XCapStyle(..),
		 XClipMask(..), XFillRule(..), XFillStyle(..), 
		 XFunction(..), XJoinStyle(..), XLineStyle(..),
		 XSubwindowMode(..), XPoint(..), XSize(..), XRect(..),
		 XArc(..), XBitmapFormat(..), XByteOrder(..),
		 XPixmapFormat(..), XVisualInfo(..), XVisualClass(..),
		 XFillContent(..), XBackingStore(..), XGravity(..),
		 XWindowClass(..), XMapState(..), XImageData(..), 
		 XImageFormat(..), XImageType(..), XDrawDirection(..),
		 XColor(..), XInputFocus(..), XGrabStatus(..),
		 XKeysym(..), XCloseDownMode(..), XScreenSaver(..))
    where

data XMaybe a {-# STRICT #-} = XSome a 		
                 	     | XNull
	      --deriving (Printers)

data XDisplay 		= XDisplay 	 --deriving (Printers)
data XScreen 		= XScreen	 --deriving (Printers)
data XWindow 		= XWindow	 --deriving (Printers)
data XGcontext 		= XGcontext	 --deriving (Printers)
data XPixmap 		= XPixmap	 --deriving (Printers)
data XColormap 		= XColormap	 --deriving (Printers)
data XCursor 		= XCursor	 --deriving (Printers)
data XFont 		= XFont		 --deriving (Printers)
data XImage 		= XImage	 --deriving (Printers)

data XError {-# STRICT #-}
              = XError String 	
                --deriving Printers
data XBitmap {-# STRICT #-}
             = XBitmap [[Int]]
instance Text(XBitmap) where
  showsPrec p x = showString "<<XBitMap>>"
   
data XKeysymTable {-# STRICT #-}
             = XKeysymTable [[Integer]]	
instance Text(XKeysymTable) where
  showsPrec p x = showString "<<XKeysymTable>>"

data XBitVec {-# STRICT #-}
             = XBitVec [Int]			
instance Text(XBitVec) where
  showsPrec p x = showString "<<XBitVec>>"

data XPixarray {-# STRICT #-}
   	     = XPixarray [[Integer]]		
instance Text(XPixarray) where
  showsPrec p x = showString "<<XPixarray>>"

data XByteVec {-# STRICT #-}
             = XByteVec [Int]
instance Text(XByteVec) where
  showsPrec p x = showString "<<XByteVec>>"


data XAtom {-# STRICT #-}
             = XAtom String 		
	--deriving (Printers)

data XProperty {-#STRICT #-}
             = XProperty [Integer]  	-- data
	                 XAtom  	-- type
                         Int    	-- format
       --deriving (Printers)

data XPixel {-# STRICT #-}
            = XPixel Integer
       --deriving (Printers)

data XDrawable {-# STRICT #-}
            = XDrawWindow XWindow 
            | XDrawPixmap XPixmap
	--deriving (Printers)

data XTime {-# STRICT #-}
            = XTime Integer 
	--deriving (Printers)

data XSwitch    = XOn
                | XOff
        --deriving (Printers)

data XWindowPlace 	= XTopPlace
			| XBottomPlace
	--deriving (Printers)

data XEventMode		= XNormalMode
			| XGrabMode
			| XUngrabMode
			| XWhileGrabbedMode
	--deriving (Printers)

data XEventKind		= XAncestorKind
			| XVirtualKind
			| XInferiorKind
			| XNonlinearKind
			| XNonlinearVirtualKind
			| XPointerKind
			| XPointerRootKind
			| XNoneKind
	--deriving (Printers)

data XWindowVisibility	= XUnobscured
			| XPartiallyObscured
			| XFullyObscured
	--deriving (Printers)

data XWindowStackMode	= XStackAbove
			| XStackBelow
			| XStackTopIf
			| XStackBottomIf
			| XStackOpposite
	--deriving (Printers)

data XPropertyState	= XNewValueProperty
			| XDeletedProperty
	--deriving (Printers)

data XMapReqType	= XModifierMapping
			| XKeyboardMapping
			| XPointerMapping
	--deriving (Printers)

data XGraphFun {-# STRICT #-}
        	= XGraphFun Int  -- major opcode
	         	    Int  -- minor opcode
	--deriving (Printers)

data XEvent {-# STRICT #-}
        	= XEvent XEventType
		         [XEventSlot]

data XEventType = 	  XKeyPressEvent
		        | XKeyReleaseEvent
	        	| XButtonPressEvent
	        	| XButtonReleaseEvent
		        | XMotionNotifyEvent
		        | XEnterNotifyEvent
		        | XLeaveNotifyEvent
		        | XFocusInEvent
			| XFocusOutEvent
            		| XKeymapNotifyEvent
            		| XMappingNotifyEvent
            		| XExposureEvent
            		| XGraphicsExposureEvent
            		| XNoExposureEvent
            		| XCirculateNotifyEvent 
            		| XConfigureNotifyEvent
            		| XCreateNotifyEvent
            		| XDestroyNotifyEvent
            		| XGravityNotifyEvent
            		| XMapNotifyEvent
            		| XReparentNotifyEvent
            		| XUnmapNotifyEvent
            		| XVisibilityNotifyEvent
            		| XCirculateRequestEvent
            		| XColormapNotifyEvent
            		| XConfigureRequestEvent
            		| XMapRequestEvent
            		| XResizeRequestEvent
            		| XClientMessageEvent
            		| XPropertyNotifyEvent
            		| XSelectionClearEvent
            		| XSelectionNotifyEvent
            		| XSelectionRequestEvent
            		| XOtherEvents
       --deriving Printers

data XEventSlot {-# STRICT #-}
                = XEventWindow XWindow		
		| XEventEventWindow XWindow	
		| XEventCode Int		
		| XEventPos XPoint		
		| XEventState XStateMask	
		| XEventTime XTime		 
		| XEventRoot XWindow		 
		| XEventRootPos XPoint		
		| XEventChild (XMaybe XWindow)	
		| XEventSameScreenP Bool	
		| XEventHintP Bool		
		| XEventMode XEventMode		
		| XEventKind XEventKind		
		| XEventFocusP Bool		
		| XEventKeymap XBitVec		
		| XEventRequest XMapReqType	
		| XEventStart Int		
		| XEventCount Int		
		| XEventRect XRect		
		| XEventDrawable XDrawable	
		| XEventXGraphFun XGraphFun	
		| XEventPlace XWindowPlace	
		| XEventBorderWidth Int		
		| XEventAboveSibling (XMaybe XWindow)
		| XEventOverrideRedirectP Bool	
		| XEventParent XWindow		
		| XEventConfigureP Bool		
		| XEventVisibility XWindowVisibility
		| XEventNewP Bool		
		| XEventInstalledP Bool		
		| XEventStackMode XWindowStackMode
		| XEventValueMask Int		
		| XEventSize XSize		
		| XEventMessage XProperty	
		| XEventPropertyState XPropertyState
		| XEventAtom XAtom		
		| XEventSelection XAtom		
		| XEventTarget XAtom		
		| XEventProperty (XMaybe XAtom)	
		| XEventRequestor XWindow
       --deriving Printers

data XEventMask {-# STRICT #-}
             = XEventMask [XEventMaskKey] 
       --deriving (Printers)

data XEventMaskKey 
		= XButton1Motion
		| XButton2Motion
		| XButton3Motion
		| XButton4Motion
		| XButton5Motion
		| XButtonMotion
                | XButtonPress
		| XButtonRelease
		| XColormapChange
		| XEnterWindow
		| XExposure
		| XFocusChange
		| XKeyPress
		| XKeyRelease
		| XKeymapState
		| XLeaveWindow
		| XOwnerGrabButton
		| XPointerMotion
		| XPointerMotionHint
		| XPropertyChange
		| XResizeRedirect
		| XStructureNotify
		| XSubstructureRedirect
		| XVisibilityChange
	  --deriving (Printers)

data XStateMask	{-# STRICT #-}
            = XStateMask [XStateMaskKey] 
        --deriving (Printers)

data XStateMaskKey
		= XShift
		| XLock
		| XControl
		| XMod1
		| XMod2
		| XMod3
		| XMod4
		| XMod5
		| XButton1
		| XButton2
		| XButton3
		| XButton4
		| XButton5
	--deriving (Printers)

data XWinAttribute {-# STRICT #-} 
		= XWinBackground XPixel 
                | XWinEventMask XEventMask 
                | XWinDepth Int 	
		| XWinBorderWidth Int 	
		| XWinClass XWindowClass 
		| XWinVisual Int 	
		| XWinBorder XFillContent 
		| XWinBackingStore XBackingStore
		| XWinBackingPlanes XPixel 
		| XWinBackingPixel XPixel 
		| XWinSaveUnder XSwitch	
		| XWinDoNotPropagateMask XEventMask
		| XWinOverrideRedirect XSwitch 
		| XWinColormap XColormap 
		| XWinCursor XCursor 	
     --deriving (Printers)

data XGCAttribute {-# STRICT #-}
		= XGCArcMode XArcMode 	
		| XGCBackground XPixel 	
		| XGCCapStyle XCapStyle 
		| XGCClipMask XClipMask 
		| XGCClipOrigin XPoint 	
		| XGCDashOffset Int 	
		| XGCDashes [Int] 	
		| XGCExposures XSwitch 	
		| XGCFillRule XFillRule 
		| XGCFillStyle XFillStyle 
		| XGCFont XFont 	
		| XGCForeground XPixel 	
		| XGCFunction XFunction 
		| XGCJoinStyle XJoinStyle 
		| XGCLineStyle XLineStyle 
		| XGCLineWidth Int 	
		| XGCPlaneMask XPixel 	
		| XGCStipple XPixmap 	
		| XGCSubwindowMode XSubwindowMode
		| XGCTile XPixmap 	
		| XGCTileOrigin XPoint 	
        --deriving (Printers)

data XImAttribute {-# STRICT #-}
		= XImBitLsbFirstP Bool 	
		| XImBitsPerPixel Int 	
		| XImBlueMask XPixel 	
		| XImByteLsbFirstP Bool 
		| XImBytesPerLine Int 	
		| XImData XImageData 	
		| XImDepth Int 		
		| XImFormat XImageFormat 
		| XImGreenMask XPixel 	
		| XImSize XSize 	
		| XImName String 	
		| XImRedMask XPixel 	
		| XImHotSpot XPoint 	
	   --deriving (Printers)

data XGrabAttribute {-# STRICT #-}
		= XGrabOwnerP Bool 	
		| XGrabSyncPointerP Bool 
		| XGrabSyncKeyboardP Bool 
		| XGrabConfineTo XWindow 
		| XGrabCursor XCursor 	
	   --deriving (Printers)

data XArcMode	= XChord
		| XPieSlice
          --deriving (Printers)

data XCapStyle	= XButt
		| XNotLast
		| XProjecting
		| XRound
	   --deriving (Printers)

data XClipMask {-# STRICT #-}
        	= XClipMaskPixmap XPixmap 
		| XClipMaskRects [XRect]
		| XClipMaskNone
	   --deriving (Printers)

data XFillRule  = XFillEvenOdd
		| XFillWinding
	   --deriving (Printers)

data XFillStyle = XFillOpaqueStippled
		| XFillSolid
		| XFillStippled
		| XFillTiled
	   --deriving (Printers)

data XFunction	= XBoole1
		| XBoole2
		| XBooleAndC1
		| XBooleAndC2
		| XBooleAnd
		| XBooleC1
		| XBooleC2
		| XBooleClr
		| XBooleEqv
		| XBooleIor
		| XBooleNand
		| XBooleNor
		| XBooleOrc1
		| XBooleOrc2
		| XBooleSet
		| XBooleXor
	   --deriving (Printers)

data XJoinStyle	= XJoinBevel
		| XJoinMiter
		| XJoinRound
	   --deriving (Printers)
 
data XLineStyle = XLineSolid
		| XLineDoubleDash
		| XLineOnOffDash
	   --deriving (Printers)

data XSubwindowMode	= XClipByChildren
			| XIncludeInferiors
	   --deriving (Printers)

-- BASIC GEOMETRY

data XPoint {-# STRICT #-} = XPoint Int Int		-- x,y
	   --deriving (Printers)

data XSize {-# STRICT #-} = XSize Int Int               -- width, height
	   --deriving (Printers)

data XRect {-# STRICT #-} = XRect Int Int Int Int       -- x, y, width, height
	   --deriving (Printers)

data XArc {-# STRICT #-} = XArc Int Int Int Int Float Float
	   --deriving (Printers)  -- x, y, width, height, angle1, angle2

data XBitmapFormat {-# STRICT #-} = XBitmapFormat Int Int Bool
          --deriving (Printers) -- unit, pad, lsb-first-p

data XByteOrder = XLsbFirst
                | XMsbFirst
		   --deriving (Printers)

data XPixmapFormat {-# STRICT #-} = XPixmapFormat Int Int Int
         --deriving (Printers) -- depth, bits-per-pixel, scanline-pad

data XVisualInfo {-# STRICT #-} = XVisualInfo 
			Int 		-- id 
                        XVisualClass 	-- class 
                        XPixel 		-- red-mask 
                        XPixel 		-- green-mask 
                        XPixel 		-- blue-mask 
                        Int 		-- bits-per-rgb
			Int 		-- colormap-entries
        --deriving (Printers)

data XVisualClass	= XDirectColor
			| XGrayScale
			| XPseudoColor
			| XStaticColor
			| XStaticGray
			| XTrueColor
        --deriving (Printers)

data XFillContent {-# STRICT #-} 
	                = XFillPixel XPixel
			| XFillPixmap XPixmap 
			| XFillNone
			| XFillParentRelative
			| XFillCopy
        --deriving (Printers)

data XBackingStore 	= XAlwaysBackStore
			| XNeverBackStore
			| XBackStoreWhenMapped
			| XBackStoreNotUseful
        --deriving (Printers)

data XGravity	= XForget
		| XStatic
		| XCenter
		| XEast
		| XNorth
		| XNorthEast
		| XNorthWest
		| XSouth
		| XSouthEast
		| XSouthWest
		| XWest
        --deriving (Printers)

data XWindowClass 	= XInputOutput
			| XInputOnly
        --deriving (Printers)

data XMapState		= XUnmapped
			| XUnviewable
			| XViewable
        --deriving (Printers)

data XImageData	{-# STRICT #-} 
                = XBitmapData [XBitmap]
		| XPixarrayData XPixarray
		| XByteVecData XByteVec
        --deriving (Printers)

data XImageFormat 	= XXyPixmapImage
			| XZPixmapImage
			| XBitmapImage
        --deriving (Printers)

data XImageType	= XImageX
		| XImageXy
		| XImageZ
        --deriving (Printers)

data XDrawDirection	= XLeftToRight
			| XRightToLeft
        --deriving (Printers)

data XColor {-# STRICT #-} = XColor Float Float Float
        --deriving (Printers)

data XInputFocus {-# STRICT #-}
                	= XFocusWindow XWindow
			| XFocusNone
			| XFocusPointerRoot
			| XFocusParent
        --deriving (Printers)

data XGrabStatus	= XAlreadyGrabbed
			| XFrozen
			| XInvalidTime
			| XNotViewable
			| XSuccess
        --deriving (Printers)


data XKeysym {-# STRICT #-} = XKeysym Integer
        --deriving (Printers)


data XCloseDownMode	= XDestroy
			| XRetainPermanent
			| XRetainTemporary
        --deriving (Printers)

data XScreenSaver {-# STRICT #-} = XScreenSaver Int Int Bool Bool
       --deriving (Printers)

{-#
ImportLispType (
   XMaybe (XSome ("not-null?", "identity", "identity"),
           XNull ("null?", "'()")),
   XError (XError ("cons-xerror", "x-error-string")),
   XBitmap (XBitmap ("mk-bitmap", "sel-bitmap")),
   XKeysymTable (XKeysymTable ("mk-keysym-table", "sel-keysym-table")),
   XBitVec (XBitVec ("mk-bitvec", "sel-bitvec")),
   XPixarray (XPixarray ("mk-pixarray", "sel-pixarray")),
   XByteVec (XByteVec ("mk-bytevec", "sel-bytevec")),
   XAtom (XAtom ("mk-atom", "sel-atom")),
   XProperty (XProperty ("mk-xproperty", "sel-xproperty-data", 
	                 "sel-xproperty-type", "sel-xproperty-format")),
   XDrawable (XDrawWindow ("xlib:window-p", "identity", "identity"),
 	      XDrawPixmap ("xlib:pixmap-p", "identity", "identity")),
   XSwitch ( XOn(":on"), XOff(":off")),
   XWindowPlace (XTopPlace (":top"), XBottomPlace (":bottom")),
   XEventMode (XNormalMode (":normal"),
               XGrabMode (":grab"),
	       XUngrabMode (":ungrab"),
	       XWhileGrabbedMode (":while-grabbed")),
   XEventKind (XAncestorKind (":ancestor"),
               XVirtualKind (":virtual"),
               XInferiorKind (":inferior"),
               XNonlinearKind (":nonlinear"),
               XNonlinearVirtualKind (":nonlinear-virtual"),
               XPointerKind (":pointer"),
               XPointerRootKind (":pointer-root"),
               XNoneKind (":none")),
   XWindowVisibility (XUnobscured (":unobscured"),
                      XPartiallyObscured (":partially-obscured"),
                      XFullyObscured (":fully-obscured")),
   XWindowStackMode (XStackAbove (":above"),
                     XStackBelow (":below"),
		     XStackTopIf (":top-if"),
		     XStackBottomIf (":bottom-if"),
		     XStackOpposite (":opposite")),
   XPropertyState (XNewValueProperty (":new-value"),
                   XDeletedProperty (":deleted")),
   XMapReqType (XModifierMapping (":modifier"),
                XKeyboardMapping (":keyboard"),
		XPointerMapping (":pointer")),
   XGraphFun (XGraphFun ("cons", "car", "cdr")),
   XEvent (XEvent ("mk-event", "sel-event-type", "sel-event-slots")),
   XEventType (XKeyPressEvent (":key-press"),
               XKeyReleaseEvent (":key-release"),
	       XButtonPressEvent (":button-press"),
	       XButtonReleaseEvent (":button-release"),
	       XMotionNotifyEvent (":motion-notify"),
	       XEnterNotifyEvent (":enter-notify"),
	       XLeaveNotifyEvent (":leave-notify"),
	       XFocusInEvent (":focus-in"),
	       XFocusOutEvent (":focus-out"),
	       XKeymapNotifyEvent (":keymap-notify"),
	       XMappingNotifyEvent (":mapping-notify"),
	       XExposureEvent (":exposure"),
	       XGraphicsExposureEvent (":graphics-exposure"),
	       XNoExposureEvent (":no-exposure"),
	       XCirculateNotifyEvent (":circulate-notify"),
	       XConfigureNotifyEvent (":configure-notify"),
	       XCreateNotifyEvent (":create-notify"),
	       XDestroyNotifyEvent (":destroy-notify"),
	       XGravityNotifyEvent (":gravity-notify"),
	       XMapNotifyEvent (":map-notify"),
	       XReparentNotifyEvent (":reparent-notify"),
	       XUnmapNotifyEvent (":unmap-notify"),
	       XVisibilityNotifyEvent (":visibility-notify"),
	       XCirculateRequestEvent (":circulate-notify"),
	       XColormapNotifyEvent (":colormap-notify"),
	       XConfigureRequestEvent (":configure-request"),
	       XMapRequestEvent (":map-request"),
	       XResizeRequestEvent (":resize-request"),
	       XClientMessageEvent (":client-message"),
	       XPropertyNotifyEvent (":property-notify"),
	       XSelectionClearEvent (":selection-clear"),
	       XSelectionNotifyEvent (":selection-notify"),
	       XSelectionRequestEvent (":selection-request"),
	       XOtherEvents (":others")),
   XEventSlot (XEventWindow ("is-window", "mk-window", "keyword-val"),
               XEventEventWindow
                  ("is-event-window", "mk-event-window", "keyword-val"),
	       XEventCode ("is-code", "mk-code", "keyword-val"),
	       XEventPos ("is-pos", "mk-pos", "keyword-val"),
	       XEventState ("is-state", "mk-state", "keyword-val"),
	       XEventTime ("is-time", "mk-time", "keyword-val"),
	       XEventRoot ("is-root", "mk-root", "keyword-val"),
	       XEventRootPos ("is-root-pos", "mk-root-pos", "keyword-val"),
	       XEventChild ("is-child", "mk-child", "keyword-val"),
	       XEventSameScreenP
                  ("is-same-screen-p", "mk-same-screen-p", "keyword-val"),
	       XEventHintP ("is-hint-p", "mk-hint-p", "keyword-val"),
	       XEventMode ("is-mode", "mk-mode", "keyword-val"),
	       XEventKind ("is-kind", "mk-kind", "keyword-val"),
	       XEventFocusP ("is-focus-p", "mk-focus-p", "keyword-val"),
	       XEventKeymap ("is-keymap", "mk-keymap", "keyword-val"),
	       XEventRequest ("is-request", "mk-request", "keyword-val"),
	       XEventStart ("is-start", "mk-start", "keyword-val"),
	       XEventCount ("is-count", "mk-count", "keyword-val"),
	       XEventRect ("is-rect", "mk-rect", "keyword-val"),
	       XEventDrawable ("is-drawable", "mk-drawable", "keyword-val"),
	       XEventXGraphFun ("is-graph-fun", "mk-graph-fun", "keyword-val"),
	       XEventPlace ("is-place", "mk-place", "keyword-val"),
	       XEventBorderWidth
                ("is-border-width", "mk-border-width", "keyword-val"),
	       XEventAboveSibling 
                ("is-above-sibling", "mk-above-sibling", "keyword-val"),
	       XEventOverrideRedirectP
                ("is-override-redirect-p", "mk-override-redirect-p", "keyword-val"),
	       XEventParent ("is-parent", "mk-parent", "keyword-val"),
	       XEventConfigureP ("is-configure-p", "mk-configure-p", "keyword-val"),
	       XEventVisibility ("is-visibility", "mk-visibility", "keyword-val"),
	       XEventNewP ("is-new-p", "mk-new-p", "keyword-val"),
	       XEventInstalledP ("is-installed-p", "mk-installed-p", "keyword-val"),
	       XEventStackMode ("is-stack-mode", "mk-stack-mode", "keyword-val"),
	       XEventValueMask ("is-value-mask", "mk-value-mask", "keyword-val"),
	       XEventSize ("is-size", "mk-size", "keyword-val"),
	       XEventMessage ("is-message", "mk-message", "keyword-val"),
	       XEventPropertyState
                 ("is-property-state", "mk-property-state", "keyword-val"),
	       XEventAtom ("is-atom", "mk-atom", "keyword-val"),
	       XEventSelection ("is-selection", "mk-selection", "keyword-val"),
	       XEventTarget ("is-target", "mk-target", "keyword-val"),
	       XEventProperty ("is-property", "mk-property", "keyword-val"),
	       XEventRequestor ("is-requestor", "mk-requestor", "keyword-val")),
   XEventMask (XEventMask ("x-make-event-mask", "x-event-mask-key-list")),
   XEventMaskKey (XButton1Motion (":button-1-motion"),
                  XButton2Motion (":button-2-motion"),
		  XButton3Motion (":button-3-motion"),
		  XButton4Motion (":button-4-motion"),
		  XButton5Motion (":button-5-motion"),
		  XButtonMotion (":button-motion"),
		  XButtonPress (":button-press"),
		  XButtonRelease (":button-release"),
		  XColormapChange (":colormap-change"),
		  XEnterWindow (":enter-window"),
		  XExposure (":exposure"),
		  XFocusChange (":focus-change"),
		  XKeyPress (":key-press"),
		  XKeyRelease (":key-release"),
		  XKeymapState (":keymap-state"),
		  XLeaveWindow (":leave-window"),
		  XOwnerGrabButton (":owner-grab-button"),
		  XPointerMotion (":pointer-motion"),
		  XPointerMotionHint (":pointer-motion-hint"),
		  XPropertyChange (":property-change"),
		  XResizeRedirect (":resize-redirect"),
		  XStructureNotify (":structure-notify"),
		  XSubstructureRedirect (":substructure-notify"),
		  XVisibilityChange (":visibility-change")),
   XStateMask (XStateMask ("x-make-state-mask", "x-state-mask-key-list")),
   XStateMaskKey (XShift (":shift"),
                  XLock (":lock"),
		  XControl (":control"),
		  XMod1 (":mod-1"),
		  XMod2 (":mod-2"),
		  XMod3 (":mod-3"),
		  XMod4 (":mod-4"),
		  XMod5 (":mod-5"),
		  XButton1 (":button-1"),
		  XButton2 (":button-2"),
		  XButton3 (":button-3"),
		  XButton4 (":button-4"),
		  XButton5 (":button-5")),
  XWinAttribute
    (XWinBackground ("is-background","mk-background","keyword-val"),
     XWinEventMask ("is-event-mask","mk-event-mask","keyword-val"),
     XWinDepth ("is-depth","mk-depth","keyword-val"),
     XWinBorderWidth ("is-border-width","mk-border-width","keyword-val"),
     XWinClass ("is-class","mk-class","keyword-val"),
     XWinVisual ("is-visual","mk-visual","keyword-val"),
     XWinBorder ("is-border","mk-border","keyword-val"),
     XWinBackingStore ("is-backing-store","mk-backing-store","keyword-val"),
     XWinBackingPlanes ("is-backing-planes","mk-backing-planes","keyword-val"),
     XWinBackingPixel ("is-backing-pixel","mk-backing-pixel","keyword-val"),
     XWinSaveUnder ("is-save-under","mk-save-under","keyword-val"),
     XWinDoNotPropagateMask ("is-do-not-propagate-mask",
	  	             "mk-do-not-propagate-mask","keyword-val"),
     XWinOverrideRedirect("is-override-redirect",
                          "mk-override-redirect","keyword-val"),
     XWinColormap ("is-colormap","mk-colormap","keyword-val"),
     XWinCursor ("is-cursor","mk-cursor","keyword-val")),
   XGCAttribute(
     XGCArcMode ("is-arc-mode","mk-arc-mode","keyword-val"),
     XGCBackground ("is-background","mk-background","keyword-val"),
     XGCCapStyle ("is-cap-style","mk-cap-style","keyword-val"),
     XGCClipMask ("is-clip-mask","mk-clip-mask","keyword-val"),
     XGCClipOrigin ("is-clip-origin","mk-clip-origin","keyword-val"),
     XGCDashOffset ("is-dash-offset","mk-dash-offset","keyword-val"),
     XGCDashes ("is-dashes","mk-dashes","keyword-val"),
     XGCExposures ("is-exposures","mk-exposures","keyword-val"),
     XGCFillRule ("is-fill-rule","mk-fill-rule","keyword-val"),
     XGCFillStyle ("is-fill-style","mk-fill-style","keyword-val"),
     XGCFont ("is-font","mk-font","keyword-val"),
     XGCForeground ("is-foreground","mk-foreground","keyword-val"),
     XGCFunction ("is-function","mk-function","keyword-val"),
     XGCJoinStyle ("is-join-style","mk-join-style","keyword-val"),
     XGCLineStyle ("is-line-style","mk-line-style","keyword-val"),
     XGCLineWidth ("is-line-width","mk-line-width","keyword-val"),
     XGCPlaneMask ("is-plane-mask","mk-plane-mask","keyword-val"),
     XGCStipple ("is-stipple","mk-stipple","keyword-val"),
     XGCSubwindowMode ("is-subwindow-mode","mk-subwindow-mode","keyword-val"),
     XGCTile ("is-tile","mk-tile","keyword-val"),
     XGCTileOrigin ("is-tile-origin","mk-tile-origin","keyword-val")),
   XImAttribute (
     XImBitLsbFirstP ("is-bit-lsb-first-p","mk-bit-lsb-first-p","keyword-val"),
     XImBitsPerPixel ("is-bits-per-pixel","mk-bits-per-pixel","keyword-val"),
     XImBlueMask ("is-blue-mask","mk-blue-mask","keyword-val"),
     XImByteLsbFirstP ("is-byte-lsb-first-p","mk-byte-lsb-first-p","keyword-val"),
     XImBytesPerLine ("is-bytes-per-line","mk-bytes-per-line","keyword-val"),
     XImData ("is-data","mk-data","keyword-val"),
     XImDepth ("is-depth","mk-depth","keyword-val"),
     XImFormat ("is-format","mk-format","keyword-val"),
     XImGreenMask ("is-green-mask","mk-green-mask","keyword-val"),
     XImSize ("is-size","mk-size","keyword-val"),
     XImName ("is-name","mk-name","keyword-val"),
     XImRedMask ("is-red-mask","mk-red-mask","keyword-val"),
     XImHotSpot ("is-hot-spot","mk-hot-spot","keyword-val")),
   XGrabAttribute (
     XGrabOwnerP ("is-owner-p", "mk-owner-p", "keyword-val"),
     XGrabSyncPointerP ("is-sync-pointer-p", "mk-sync-pointer-p", "keyword-val"),
     XGrabSyncKeyboardP ("is-sync-keyboard-p", "mk-sync-keyboard-p", "keyword-val"),
     XGrabConfineTo ("is-confine-to", "mk-confine-to", "keyword-val"),
     XGrabCursor ("is-cursor", "mk-cursor", "keyword-val")),
   XArcMode (XChord (":chord"),
             XPieSlice (":pie-slice")),
   XCapStyle (XButt (":butt"),
              XNotLast (":not-last"),
	      XProjecting (":projecting"),
	      XRound (":round")),
   XClipMask (XClipMaskPixmap ("xlib:pixmap-p","identity","identity"),
	      XClipMaskRects ("not-pixmap-and-list-p","mk-clip-mask-rects",
	                      "sel-clip-mask-rects"),
	      XClipMaskNone ("null?", "()")),
   XFillRule (XFillEvenOdd (":even-odd"),
              XFillWinding (":winding")),
   XFillStyle (XFillOpaqueStippled (":opaque-stippled"),
               XFillSolid (":solid"),
	       XFillStippled (":stippled"),
	       XFillTiled (":tiled")),
   XFunction (XBoole1 ("xlib::boole-1"),
              XBoole2 ("xlib::boole-2"),
	      XBooleAndC1 ("xlib::boole-andc1"),
	      XBooleAndC2 ("xlib::boole-andc2"),
	      XBooleAnd ("xlib::boole-and"),
	      XBooleC1 ("xlib::boole-c1"),
	      XBooleC2 ("xlib::boole-c2"),
	      XBooleClr ("xlib::boole-clr"),
	      XBooleEqv ("xlib::boole-eqv"),
	      XBooleIor ("xlib::boole-ior"),
	      XBooleNand ("xlib::boole-nand"),
	      XBooleNor ("xlib::boole-nor"),
	      XBooleOrc1 ("xlib::boole-orc1"),
	      XBooleOrc2 ("xlib::boole-orc2"),
	      XBooleSet ("xlib::boole-set"),
	      XBooleXor ("xlib::boole-xor")),
   XJoinStyle (XJoinBevel (":bevel"),
               XJoinMiter (":miter"),
	       XJoinRound (":round")),
   XLineStyle (XLineSolid (":solid"),
               XLineDoubleDash (":double-dash"),
	       XLineOnOffDash (":on-off-dash")),
   XSubwindowMode (XClipByChildren (":clip-by-children"),
   	           XIncludeInferiors (":include-inferiors")),
   XPoint(XPoint("mk-xpoint", "xpoint-x", "xpoint-y")),
   XSize (XSize ("mk-xsize", "xsize-w", "xsize-h")),
   XRect (XRect ("mk-xrect", "xrect-x", "xrect-y", "xrect-w", "xrect-h")),
   XArc	(XArc ("mk-xarc", "xarc-x", "xarc-y", "xarc-w", "xarc-h",
	       "xarc-a1", "xarc-a2")),
   XBitmapFormat 
	(XBitmapFormat ("bitmap-format-p", "mk-bitmap-format",
			"xlib:bitmap-format-unit",
			"xlib:bitmap-format-pad",
			"xlib:bitmap-format-lsb-first-p")),
   XByteOrder (XLsbFirst (":lsbfirst"),
               XMsbFirst (":msbfirst")),
   XPixmapFormat (XPixmapFormat ("pixmap-format-p", "mk-pixmap-format", 
			         "xlib:pixmap-format-depth",
			         "xlib:pixmap-format-bits-per-pixel",
			         "xlib:pixmap-format-scanline-pad")),
   XVisualInfo 
	(XVisualInfo (	"visual-info-p", "mk-xvisual-info", 
		      	"xlib:visual-info-id", 
		      	"xlib:visual-info-class",
		      	"xlib:visual-info-red-mask",
			"xlib:visual-info-green-mask",
			"xlib:visual-info-blue-mask",
 			"xlib:visual-info-bits-per-rgb", 
			"xlib:visual-info-colormap-entries")),
   XVisualClass (XDirectColor (":direct-color"),
	         XGrayScale (":gray-scale"),
		 XPseudoColor (":pseudo-color"),
		 XStaticColor (":static-color"),
		 XStaticGray  (":static-gray"),
		 XTrueColor   (":true-color")),
   XFillContent (XFillPixel  ("is-fill-pixel", "identity","identity"),
   	         XFillPixmap ("xlib:pixmap-p", "identity","identity"),
		 XFillNone (":none"),
		 XFillParentRelative (":parent-relative"),
		 XFillCopy (":copy")),
   XBackingStore (XAlwaysBackStore (":always"),
                  XNeverBackStore (":never"),
                  XBackStoreWhenMapped (":when-mapped"),
		  XBackStoreNotUseful (":not-useful")),
   XGravity (XForget (":forget"),
             XStatic (":static"),
	     XCenter (":center"),
	     XEast (":east"),
	     XNorth (":north"),
	     XNorthEast (":north-east"),
	     XNorthWest (":north-west"),
	     XSouth (":south"),
	     XSouthEast (":south-east"),
	     XSouthWest (":south-west"),
	     XWest ("west")),
   XWindowClass (XInputOutput (":input-output"),
                 XInputOnly (":input-only")),
   XMapState (XUnmapped (":unmapped"),
              XUnviewable (":unviewable"),
	      XViewable (":viewable")),
   XImageData (XBitmapData ("bitmap-list-p", "haskell-list->list/identity", "list->haskell-list/identity"),
	       XPixarrayData ("pixarray-p", "identity", "identity"),
	       XByteVecData ("bytevec-p", "identity", "identity")),
   XImageFormat (XXyPixmapImage (":xy-pixmap"),
                 XZPixmapImage (":z-pixmap"),
		 XBitmapImage (":bitmap")),
   XImageType (XImageX ("'xlib:image-x"),
               XImageXy ("'xlib:image-xy"),
	       XImageZ ("'xlib:image-z")),
   XDrawDirection (XLeftToRight (":left-to-right"),
	           XRightToLeft (":right-to-left")),
   XColor (XColor ("xlib:color-p", "mk-color", 
                   "xlib:color-red", "xlib:color-green", "xlib:color-blue")),
   XInputFocus (XFocusWindow ("xlib:window-p", "identity", "identity"),
                XFocusNone (":none"),
		XFocusPointerRoot (":pointer-root"),
		XFocusParent (":parent")),
   XGrabStatus (XAlreadyGrabbed (":already-grabbed"),
                XFrozen (":frozen"),
                XInvalidTime (":invalid-time"),
		XSuccess (":success")),
   XCloseDownMode (XDestroy (":destroy"),
	           XRetainPermanent (":retain-permanent"),
		   XRetainTemporary (":retain-temporary")),
   XScreenSaver (XScreenSaver ("list", "car", "cadr", "caddr", "cadddr")))

#-}

