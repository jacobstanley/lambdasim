{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lambdasim.Simulation where

import Control.Parallel.Strategies
import Data.DeriveTH
import Data.Record.Label
import Data.Time (UTCTime)
import Text.Printf (printf)

import Prelude ()
import Lambdasim.Prelude
import Lambdasim.Geographical
import Lambdasim.Time


data Simulation = Simulation {
  _time :: UTCTime,
  _vessels :: [Vessel]
}

data Vessel = Vessel {
  _position :: Geog,
  _heading :: Angle,
  _rudder :: AngularVelocity,
  _speed :: Velocity
}

$(derive makeNFData ''Simulation)
$(derive makeNFData ''Vessel)

$(mkLabels [''Simulation, ''Vessel])

vessels :: Simulation :-> [Vessel]
time    :: Simulation :-> UTCTime

position :: Vessel :-> Geog
heading  :: Vessel :-> Angle
rudder   :: Vessel :-> AngularVelocity
speed    :: Vessel :-> Velocity


class AdvanceTime a where
  advanceBy :: Time -> a -> a


instance Show Simulation where
  show s = printf "Simulation\n\
                  \  Time: %s\n\
                  \  Vessels: %s" t v
    where t = show (get time s)
          v = show (get vessels s)

instance AdvanceTime Simulation where
  advanceBy t s = set time (addTime t $ get time s) $
                  set vessels (map (advanceBy t) (get vessels s)) s

newSimulation :: UTCTime -> Simulation
newSimulation utc = Simulation {
  _time = utc,
  _vessels = []
}

addVessel :: Simulation -> Simulation
addVessel = updateVessels (\vs -> newVessel : vs)

updateVessels :: ([Vessel] -> [Vessel]) -> Simulation -> Simulation
updateVessels f s = set vessels (f $ get vessels s) s

updateFirstVessel :: (Vessel -> Vessel) -> Simulation -> Simulation
updateFirstVessel f = updateVessels update
  where update [] = []
        update (v:vs) = f v : vs


instance Show Vessel where
  show v = printf "Vessel Pos: %s Hdg: %.2f deg" p h
    where h = get heading v /~ degree
          p = show (get position v)

instance AdvanceTime Vessel where
  advanceBy t v = set position (translate dst hdg pos) $
                  set heading (normalize360 $ hdg + (rdr * t)) v
    where pos = get position v
          hdg = get heading v
          rdr = get rudder v
          spd = get speed v
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
  _position = mkGeog (-32) 116 0,
  _heading = 0 *~ degree,
  _rudder  = 2 *~ (degree / second),
  _speed   = 5 *~ knot
}
