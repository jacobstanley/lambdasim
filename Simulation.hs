module Simulation where

import Geographical
import Primitives
import Time

import qualified Prelude
import Control.Concurrent (threadDelay)
import Data.Time
import Numeric.Units.Dimensional.Prelude
import Text.Printf (printf)

timeStep :: Time'
timeStep = 10 *~ milli second

forkSim :: IO Simulation
forkSim = do
  time <- getCurrentTime
  let sim = addVessel $ mkSim $ toNearestSecond time
  forkIO (simulate sim)
  return sim

simulate :: Simulation -> IO ()
simulate s = do
  s' <- advanceTo =<< getCurrentTime
  threadDelay 1000
  simulate s'
  where newTimeFor = addTime timeStep
        advanceTo t
          | simTime s > newTimeFor t = return s
          | otherwise = do
              let s' = advanceSim timeStep s
              putStrLn $ show s'
              return s'

data Simulation = Simulation {
  simTime :: UTCTime,
  simVessels :: [Vessel]
}

instance Show Simulation where
  show s = printf "Simulation\n\
                  \  Time: %s\n\
                  \  Vessels: %s" t v
    where t = show (simTime s)
          v = show (simVessels s)

mkSim :: UTCTime -> Simulation
mkSim utc = Simulation {
  simTime = utc,
  simVessels = []
}

addVessel :: Simulation -> Simulation
addVessel s = s { simVessels = mkVessel : (simVessels s) }

advanceSim :: Time' -> Simulation -> Simulation
advanceSim t s =
  s { simTime = addTime t (simTime s),
      simVessels = map (advanceVessel t) (simVessels s) }


data Vessel = Vessel {
  vesPosition :: Geog,
  vesHeading :: Angle',
  vesRudder :: AngularVelocity',
  vesSpeed :: Velocity'
}

instance Show Vessel where
  show v = printf "Vessel Pos: %s Hdg: %.2f deg" p h
    where h = (vesHeading v) /~ degree
          p = show (vesPosition v)

mkVessel :: Vessel
mkVessel = Vessel {
  vesPosition = mkGeog 32 116,
  vesHeading = 0 *~ degree,
  vesRudder  = 2 *~ (degree / second),
  vesSpeed   = 5 *~ knot
}

advanceVessel :: Time' -> Vessel -> Vessel
advanceVessel t v = v {
  vesPosition = translate dst hdg pos,
  vesHeading = hdg + (rdr * t)
} where pos = vesPosition v
        hdg = vesHeading v
        rdr = vesRudder v
        spd = vesSpeed v
        dst = spd * t
