{-# LANGUAGE TemplateHaskell #-}

module Simulation where

import Geographical
import Primitives
import Time

import qualified Prelude
import Data.Time
import Data.DeriveTH
import Data.Derive.NFData
import Text.Printf (printf)
import Control.Parallel.Strategies
import Numeric.Units.Dimensional.Prelude

class AdvanceTime a where
  advanceBy :: Time' -> a -> a

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

instance AdvanceTime Simulation where
  advanceBy t s =
    s { simTime = addTime t (simTime s),
        simVessels = map (advanceBy t) (simVessels s) }

newSimulation :: UTCTime -> Simulation
newSimulation utc = Simulation {
  simTime = utc,
  simVessels = []
}

addVessel :: Simulation -> Simulation
addVessel = updateVessels (\vs -> newVessel : vs)

updateVessels :: ([Vessel] -> [Vessel]) -> Simulation -> Simulation
updateVessels f s = s { simVessels = f (simVessels s) }

updateFirstVessel :: (Vessel -> Vessel) -> Simulation -> Simulation
updateFirstVessel f = updateVessels update
  where update [] = []
        update (v:vs) = f v : vs

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

instance AdvanceTime Vessel where
  advanceBy t v =
    v { vesPosition = translate dst hdg pos,
        vesHeading = hdg + (rdr * t) }
    where pos = vesPosition v
          hdg = vesHeading v
          rdr = vesRudder v
          spd = vesSpeed v
          dst = spd * t

newVessel :: Vessel
newVessel = Vessel {
  vesPosition = mkGeog 32 116,
  vesHeading = 0 *~ degree,
  vesRudder  = 2 *~ (degree / second),
  vesSpeed   = 5 *~ knot
}

$(derive makeNFData ''Vessel)
$(derive makeNFData ''Simulation)
