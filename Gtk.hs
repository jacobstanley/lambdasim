import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad(liftM)

main :: IO ()
main = do
  unsafeInitGUIForThreadedRTS
  Just xml <- xmlNew "main.glade"
  
  window  <- xmlGetWidget xml castToWindow "window"
  
  heading <- xmlGetWidget xml castToHScale "headingScale"
  headingAdj <- adjustmentNew 0 0 360 10 30 0
  rangeSetAdjustment heading headingAdj
  
  rudder <- xmlGetWidget xml castToHScale "rudderScale"
  rudderAdj <- adjustmentNew 0 (-20) (20) 1 5 0
  rangeSetAdjustment rudder rudderAdj
  
  speed <- xmlGetWidget xml castToHScale "speedScale"
  speedAdj <- adjustmentNew 0 (-5) (20) 1 5 0
  rangeSetAdjustment speed speedAdj

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
