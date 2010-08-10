module Lambdasim.SimRunner (
    startSimulation,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Parallel.Strategies
import Data.Record.Label
import Data.Time (UTCTime, getCurrentTime)

import Prelude ()
import Lambdasim.Prelude
import Lambdasim.NMEA
import Lambdasim.Simulation
import Lambdasim.STM
import Lambdasim.Time
import Lambdasim.Udp


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
  stmUpdate_ (advanceTo t) sim
  sleep (1 *~ milli second)

monitorUdp :: TVar Simulation -> IO ()
monitorUdp sim = forever $ do
  s <- stmRead sim
  sendMsg "127.0.0.1" "8000" (toNMEA s)
  sleep (1 *~ second)

toNMEA :: Simulation -> String
toNMEA sim = gga utc pos GPS
  where utc = get simTime sim
        pos = get vesPosition $ head $ get simVessels sim

sleep :: Time -> IO ()
sleep t = threadDelay us
  where us = round (t /~ micro second)

advanceTo :: UTCTime -> Simulation -> Simulation
advanceTo t s
  | isLater   = s
  | otherwise = rnf s' `seq` s'
  where isLater = currentTime > proposedTime
        currentTime = get simTime s
        proposedTime = addTime timeStep t
        s' = advanceBy timeStep s

timeStep :: Time
timeStep = 5 *~ milli second
