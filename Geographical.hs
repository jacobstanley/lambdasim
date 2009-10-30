module Geographical where

import qualified Prelude
import Primitives
import Numeric.Units.Dimensional.Prelude
import Text.Printf (printf)

data Geog = Geog Lat Lon Elh

instance Show Geog where
  show pos = printf "%.6f' %.6f'" lat' lon'
    where Geog (Lat lat) (Lon lon) _ = pos
          lat' = lat /~ degree
          lon' = lon /~ degree

newtype Lat = Lat Angle'
newtype Lon = Lon Angle'
newtype Elh = Elh Length'

mkGeog :: Double -> Double -> Geog
mkGeog lat lon =
  Geog (Lat $ lat *~ degree)
       (Lon $ lon *~ degree)
       (Elh $ 0 *~ metre)

earthRadius :: Length'
earthRadius = 6378137 *~ meter

translate :: Length' -> Angle' -> Geog -> Geog
translate dst hdg (Geog (Lat lat) (Lon lon) elh) = pos'
  where pos' = Geog (Lat lat') (Lon lon') elh
        angDst = dst / earthRadius
        lat' = asin (sin lat * cos angDst +
                     cos lat * sin angDst * cos hdg)
        lon' = lon + atan2 (sin hdg    * sin angDst * cos lat)
                           (cos angDst - sin lat    * sin lat')
