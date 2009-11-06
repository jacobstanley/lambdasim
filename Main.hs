module Main where

import Gtk
import Primitives
import Simulation
import STM
import Time

import Data.Time
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf(printf)
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

main :: IO ()
main = do
  unsafeInitGUIForThreadedRTS
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100

  Just xml <- xmlNew "main.glade"

  window <- xmlGetWidget xml castToWindow "window"
  onDestroy window mainQuit

  headingScale <- xmlGetWidget xml castToHScale "heading"
  rudderScale  <- xmlGetWidget xml castToHScale "rudder"
  speedScale   <- xmlGetWidget xml castToHScale "speed"

  sim <- startSimulation

  onRangeValueChanged headingScale $ do
    heading <- rangeGetValue headingScale
    stmApply (setHeading (heading *~ degree)) sim

  onRangeValueChanged rudderScale $ do
    rudder <- rangeGetValue rudderScale
    stmApply (setRudder (rudder *~ (degree / second))) sim

  onInterval 100 $ do
    vessel <- head <$> simVessels <$> (stmRead sim)
    rangeSetValue headingScale $ (vesHeading vessel) /~ degree
    rangeSetValue rudderScale $ (vesRudder vessel) /~ (degree / second)
    rangeSetValue speedScale $ (vesSpeed vessel) /~ knot
    mapM_ widgetQueueDraw [headingScale, rudderScale, speedScale]

  widgetShowAll window
  mainGUI

setHeading :: Angle' -> Simulation -> Simulation
setHeading x = updateFirstVessel (\v -> v { vesHeading = x })

setRudder :: AngularVelocity' -> Simulation -> Simulation
setRudder x = updateFirstVessel (\v -> v { vesRudder = x })

setSpeed :: Velocity' -> Simulation -> Simulation
setSpeed x = updateFirstVessel (\v -> v { vesSpeed = x })

startSimulation :: IO (TVar Simulation)
startSimulation = do
  time <- getRoundedTime
  sim <- stmNew $ addVessel (newSimulation time)
  forkIO (simulate sim)
  return sim

simulate :: TVar Simulation -> IO ()
simulate sim = forever $ do
  t <- getCurrentTime
  stmApply (advanceTo t) sim
  sleep (1 *~ milli second)

monitor :: TVar Simulation -> IO ()
monitor sim = forever $ do
  s <- stmRead sim
  putStrLn (show s)
  sleep (1 *~ second)

sleep :: Time' -> IO ()
sleep t = threadDelay us
  where us = round (t /~ micro second)

advanceTo :: UTCTime -> Simulation -> Simulation
advanceTo t s
  | currentTime > step t = s
  | otherwise = advanceBy timeStep s
  where step = addTime timeStep
        currentTime = simTime s

timeStep :: Time'
timeStep = 10 *~ milli second
