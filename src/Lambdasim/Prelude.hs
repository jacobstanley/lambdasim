{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambdasim.Prelude (
    module Control.Category,
    module Numeric.Units.Dimensional,
    module Numeric.Units.Dimensional.SIUnits,
    module Prelude,

    Unit,
    Quantity,

    Angle,           DAngle,
    AngularVelocity, DAngularVelocity,
    Length,          DLength,
    Time,            DTime,
    Velocity,        DVelocity,

    knot
) where

import           Control.Category
import           Control.Parallel.Strategies
import qualified Numeric.Units.Dimensional as D
import qualified Numeric.Units.Dimensional.Quantities as Q
import           Numeric.Units.Dimensional.SIUnits
import           Numeric.Units.Dimensional.NonSI (nauticalMile)

import Numeric.Units.Dimensional
  ( (/~), (*~), (*), (/), (^), (^+), negate, (+), (-), abs, nroot
  , sqrt, cbrt, (^/), (*~~), (/~~), sum, dimensionlessLength, log, sin
  , cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
  , exp, (**), atan2, one, pi, dimUnit, prefix
  )

import Prelude hiding
  ( (*), (/), (^), negate, (+), (-), abs
  , sqrt, log, sin, sum
  , cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
  , exp, (**), atan2, pi
  , (.), id
  )


type Unit d     = D.Dimensional D.DUnit d Double
type Quantity d = D.Dimensional D.DQuantity d Double

type DAngle           = Q.DPlaneAngle
type DAngularVelocity = Q.DAngularVelocity
type DLength          = D.DLength
type DTime            = D.DTime
type DVelocity        = Q.DVelocity

type Angle           = Quantity DAngle
type AngularVelocity = Quantity DAngularVelocity
type Length          = Quantity DLength
type Time            = Quantity DTime
type Velocity        = Quantity DVelocity

knot :: Unit DVelocity
knot = nauticalMile / hour

instance NFData a => NFData (D.Dimensional v d a) where
    rnf (D.Dimensional x) = rnf x
