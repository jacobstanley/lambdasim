module Main where

import Simulation
import Time

import Data.Time
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad (liftM)
import Control.Concurrent

--main = do
--  time <- getCurrentTime
--  simulate $ addVessel $ mkSim $ toNearestSecond time

main :: IO ()
main = do
  unsafeInitGUIForThreadedRTS
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100

  Just xml <- xmlNew "main.glade"

  window <- xmlGetWidget xml castToWindow "window"
  onDestroy window mainQuit

  heading <- xmlGetWidget xml castToHScale "heading"
  rudder  <- xmlGetWidget xml castToHScale "rudder"
  speed   <- xmlGetWidget xml castToHScale "speed"

  sim <- forkSim

  onRangeValueChanged heading $ do
    value <- rangeGetValue heading
    putStrLn $ "Heading: " ++ (show value)

  widgetShowAll window
  mainGUI

