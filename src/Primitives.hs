{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primitives (
    module Numeric.Units.Dimensional.Prelude,
    Primitives.Angle,
    Primitives.AngularVelocity,
    Primitives.Length,
    Primitives.Time,
    Primitives.Velocity,
    knot
) where

import           Control.Parallel.Strategies
import           Numeric.Units.Dimensional
import qualified Numeric.Units.Dimensional.Prelude as D
import           Numeric.Units.Dimensional.Prelude hiding
                 (Angle,AngularVelocity,Length,Time,Velocity)
import           Numeric.Units.Dimensional.NonSI (nauticalMile)
import           Prelude ()


type Angle           = D.Angle Double
type AngularVelocity = D.AngularVelocity Double
type Length          = D.Length Double
type Time            = D.Time Double
type Velocity        = D.Velocity Double

knot :: Fractional a => Unit DVelocity a
knot = nauticalMile / hour

instance NFData a => NFData (Dimensional v d a) where
    rnf (Dimensional x) = rnf x
