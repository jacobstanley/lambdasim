module Main where

import NMEA
import Primitives
import Simulation
import STM
import Time
import UdpSender

import Data.Time
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
  sim <- startSimulation


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
