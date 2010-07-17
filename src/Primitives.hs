{-# LANGUAGE StandaloneDeriving #-}

module Primitives where

import Control.Parallel.Strategies
import Data.Data
import Numeric.NumType (Zero, Pos, Neg)
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

deriving instance Typeable3 Dimensional
deriving instance Typeable  DQuantity
deriving instance Typeable7 Dim
deriving instance Typeable  Zero
deriving instance Typeable1 Pos
deriving instance Typeable1 Neg
deriving instance (Typeable v, Typeable d, Data a) => Data (Dimensional v d a)

instance NFData a => NFData (Dimensional v d a) where
  rnf (Dimensional x) = rnf x
