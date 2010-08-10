module Lambdasim.SimRunner (
    startSimulation,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Parallel.Strategies
import Data.Record.Label
import Data.Time (UTCTime, getCurrentTime)
import Network.Info

import Prelude ()
import Lambdasim.Prelude
import Lambdasim.NMEA
import Lambdasim.Simulation
import Lambdasim.STM
import Lambdasim.Time
import Lambdasim.Udp


startSimulation :: IO (TVar Simulation)
startSimulation = do
    t <- getRoundedTime
    s <- stmNew $ addVessel (newSimulation t)
    hosts <- getLocalIPs
    forkIO (simulate s)
    forkIO (monitorUdp s hosts)
    return s

simulate :: TVar Simulation -> IO ()
simulate sim = forever $ do
    t <- getCurrentTime
    stmUpdate_ (advanceTo t) sim
    sleep (1 *~ milli second)


type HostName = String

getLocalIPs :: IO [HostName]
getLocalIPs = liftM extract getNetworkInterfaces
  where
    extract  = map show . filter (/= nullIPv4) . map ipv4
    nullIPv4 = IPv4 0

monitorUdp :: TVar Simulation -> [HostName] -> IO ()
monitorUdp sim hosts = forever $ do
    s <- stmRead sim
    send "8000" (toNMEA s)
    sleep (1 *~ second)
  where
    send p x = mapM_ (\h -> sendMsg h p x) hosts

toNMEA :: Simulation -> String
toNMEA sim = gga utc pos GPS
  where
    utc = get time sim
    pos = get position $ head $ get vessels sim

sleep :: Time -> IO ()
sleep t = threadDelay us
  where
    us = round (t /~ micro second)


advanceTo :: UTCTime -> Simulation -> Simulation
advanceTo t s
    | isLater   = s
    | otherwise = rnf s' `seq` s'
  where
    isLater = currentTime > proposedTime
    currentTime = get time s
    proposedTime = addTime timeStep t
    s' = advanceBy timeStep s

timeStep :: Time
timeStep = 5 *~ milli second
