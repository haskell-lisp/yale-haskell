module Test where
import Xlib

xGetEventMul :: XMArray XDisplay -> IO (Int, XEvent)
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

