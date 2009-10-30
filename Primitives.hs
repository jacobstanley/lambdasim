module Primitives where

import qualified Prelude
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (nauticalMile)

type Angle'           = Angle Double
type AngularVelocity' = AngularVelocity Double
type Length'          = Length Double
type Time'            = Time Double
type Velocity'        = Velocity Double

knot :: Fractional a => Unit DVelocity a
knot = nauticalMile / hour
