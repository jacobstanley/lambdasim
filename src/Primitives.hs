module Primitives where

import Control.Parallel.Strategies
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (nauticalMile)
import Prelude hiding ((/))

type Angle'           = Angle Double
type AngularVelocity' = AngularVelocity Double
type Length'          = Length Double
type Time'            = Time Double
type Velocity'        = Velocity Double

knot :: Fractional a => Unit DVelocity a
knot = nauticalMile / hour

instance NFData a => NFData (Dimensional v d a) where
  rnf (Dimensional x) = rnf x
