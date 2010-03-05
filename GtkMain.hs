module Main where

import Gtk
import NMEA
import Primitives
import Simulation
import STM
import Time
import UdpSender

import Data.Time
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Parallel.Strategies
import Text.Printf(printf)
import Numeric.Units.Dimensional.Prelude
import Prelude hiding ((/))

main :: IO ()
main = do
  unsafeInitGUIForThreadedRTS
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100

  Just xml <- xmlNew "lambdasim.glade"

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

  onRangeValueChanged speedScale $ do
    speed <- rangeGetValue speedScale
    stmApply (setSpeed (speed *~ knot)) sim

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
  forkIO (monitorUdp sim)
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

monitorUdp :: TVar Simulation -> IO ()
monitorUdp sim = forever $ do
  s <- stmRead sim
  sendMsg "127.0.0.1" "2000" (toNMEA s)
  sleep (1 *~ second)

toNMEA :: Simulation -> String
toNMEA sim = gga utc pos GPS
  where utc = simTime sim
        pos = vesPosition $ head $ simVessels sim

sleep :: Time' -> IO ()
sleep t = threadDelay us
  where us = round (t /~ micro second)

advanceTo :: UTCTime -> Simulation -> Simulation
advanceTo t s
  | isLater   = s
  | otherwise = rnf s' `seq` s'
  where isLater = currentTime > proposedTime
        currentTime = simTime s
        proposedTime = addTime timeStep t
        s' = advanceBy timeStep s

timeStep :: Time'
timeStep = 5 *~ milli second
