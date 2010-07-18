module SimRunner (
    startSimulation,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Parallel.Strategies
import Data.Time (UTCTime, getCurrentTime)

import NMEA
import Primitives
import Simulation
import STM
import Time
import UdpSender


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

monitorUdp :: TVar Simulation -> IO ()
monitorUdp sim = forever $ do
  s <- stmRead sim
  sendMsg "127.0.0.1" "8000" (toNMEA s)
  sleep (1 *~ second)

toNMEA :: Simulation -> String
toNMEA sim = gga utc pos GPS
  where utc = simTime sim
        pos = vesPosition $ head $ simVessels sim

sleep :: Time -> IO ()
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

timeStep :: Time
timeStep = 5 *~ milli second
