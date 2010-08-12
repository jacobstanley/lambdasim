module Lambdasim.Geographical (
    Geog(..),
    mkGeog,
    translate,
) where

import Control.Parallel.Strategies
import Text.Printf (printf)

import Prelude ()
import Lambdasim.Prelude


type Latitude = Angle
type Longitude = Angle
type EllipsoidalHeight = Length

data Geog = Geog Latitude Longitude EllipsoidalHeight

instance Show Geog where
    show (Geog lat lon _) =
        printf "%.6f' %.6f'" (lat /~ degree) (lon /~ degree)

mkGeog :: Double -> Double -> Double -> Geog
mkGeog lat lon elh = Geog (lat *~ degree)
                          (lon *~ degree)
                          (elh *~ metre)

earthRadius :: Length
earthRadius = 6378137 *~ meter

translate :: Length -> Angle -> Geog -> Geog
translate dst hdg (Geog lat lon elh) = pos'
  where
    pos' = Geog lat' lon' elh
    angDst = dst / earthRadius
    lat' = asin (sin lat * cos angDst +
                 cos lat * sin angDst * cos hdg)
    lon' = lon + atan2 (sin hdg    * sin angDst * cos lat)
                       (cos angDst - sin lat    * sin lat')

instance NFData Geog where
    rnf (Geog lat lon elh) = rnf lat `seq` rnf lon `seq` rnf elh
