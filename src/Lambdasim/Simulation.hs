{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lambdasim.Simulation where

import Control.Parallel.Strategies
import Data.DeriveTH
import Data.Record.Label
import Data.Time (UTCTime)
import Prelude ()
import Text.Printf (printf)

import Lambdasim.Geographical
import Lambdasim.Primitives
import Lambdasim.Time


data Simulation = Simulation {
  _simTime :: UTCTime,
  _simVessels :: [Vessel]
}

data Vessel = Vessel {
  _vesPosition :: Geog,
  _vesHeading :: Angle,
  _vesRudder :: AngularVelocity,
  _vesSpeed :: Velocity
}

$(derive makeNFData ''Simulation)
$(derive makeNFData ''Vessel)

$(mkLabels [''Simulation, ''Vessel])

simVessels :: Simulation :-> [Vessel]
simTime    :: Simulation :-> UTCTime

vesPosition :: Vessel :-> Geog
vesHeading  :: Vessel :-> Angle
vesRudder   :: Vessel :-> AngularVelocity
vesSpeed    :: Vessel :-> Velocity


class AdvanceTime a where
  advanceBy :: Time -> a -> a


instance Show Simulation where
  show s = printf "Simulation\n\
                  \  Time: %s\n\
                  \  Vessels: %s" t v
    where t = show (get simTime s)
          v = show (get simVessels s)

instance AdvanceTime Simulation where
  advanceBy t s = set simTime (addTime t $ get simTime s) $
                  set simVessels (map (advanceBy t) (get simVessels s)) s

newSimulation :: UTCTime -> Simulation
newSimulation utc = Simulation {
  _simTime = utc,
  _simVessels = []
}

addVessel :: Simulation -> Simulation
addVessel = updateVessels (\vs -> newVessel : vs)

updateVessels :: ([Vessel] -> [Vessel]) -> Simulation -> Simulation
updateVessels f s = set simVessels (f $ get simVessels s) s

updateFirstVessel :: (Vessel -> Vessel) -> Simulation -> Simulation
updateFirstVessel f = updateVessels update
  where update [] = []
        update (v:vs) = f v : vs


instance Show Vessel where
  show v = printf "Vessel Pos: %s Hdg: %.2f deg" p h
    where h = get vesHeading v /~ degree
          p = show (get vesPosition v)

instance AdvanceTime Vessel where
  advanceBy t v = set vesPosition (translate dst hdg pos) $
                  set vesHeading (normalize360 $ hdg + (rdr * t)) v
    where pos = get vesPosition v
          hdg = get vesHeading v
          rdr = get vesRudder v
          spd = get vesSpeed v
          dst = spd * t

normalize360 :: Angle -> Angle
normalize360 x | x <  deg0   = normalize360 (x + deg360)
               | x >= deg360 = normalize360 (x - deg360)
               | otherwise   = x
  where
    deg0 = 0.0 *~ degree
    deg360 = 360.0 *~ degree

newVessel :: Vessel
newVessel = Vessel {
  _vesPosition = mkGeog (-32) 116 0,
  _vesHeading = 0 *~ degree,
  _vesRudder  = 2 *~ (degree / second),
  _vesSpeed   = 5 *~ knot
}
