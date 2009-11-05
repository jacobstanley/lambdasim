module Main where

import Primitives
import Simulation
import Time

import Data.Time
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad (liftM)
import Control.Concurrent
import Control.Concurrent.STM
import Numeric.Units.Dimensional.Prelude

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

  sim <- startSimulation

  onRangeValueChanged heading $ do
    value <- rangeGetValue heading
    putStrLn $ "Heading: " ++ (show value)

  widgetShowAll window
  mainGUI

timeStep :: Time'
timeStep = 10 *~ milli second

startSimulation :: IO (TVar Simulation)
startSimulation = do
  time <- getRoundedTime
  sim <- atomically $ newTVar $ addVessel (mkSim time)
  forkIO (simulate sim)
  forkIO (monitor sim)
  return sim

simulate :: TVar Simulation -> IO ()
simulate sim = do
  t <- getCurrentTime
  atomicApply (advanceTo t) sim
  sleep (10 *~ milli second)
  simulate sim

monitor :: TVar Simulation -> IO ()
monitor sim = do
  s <- atomicRead sim
  putStrLn (show s)
  sleep (1 *~ second)
  monitor sim

sleep :: Time' -> IO ()
sleep t = threadDelay us
  where us = round (t /~ micro second)

advanceTo :: UTCTime -> Simulation -> Simulation
advanceTo t s
  | currentTime > step t = s
  | otherwise = advanceSimBy timeStep s
  where step = addTime timeStep
        currentTime = simTime s
