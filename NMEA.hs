module NMEA where

import Primitives
import Geographical

import Data.Time.Clock
import Data.Char(ord)
import Data.Bits(xor)
import Data.List
import Numeric(showHex)
import Text.Printf(printf)
import System.Locale(defaultTimeLocale)
import Data.Time.Format(formatTime)
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits
import Prelude as P ((+))
import Prelude hiding ((-),(+),abs)

data FixQuality
  = Invalid | GPS | DGPS | PPS  | RTK | FloatRTK
  | DeadReckoning | Manual | Simulation
  deriving (Enum, Show)

type Satellites = Int
type HDOP = Double

gga :: UTCTime -> Geog -> FixQuality -> String
gga time (Geog lat lon elh) quality = nmea
  [ "GPGGA"
  , formatTime defaultTimeLocale "%H%M%S" time
  , latDecimalMinutes lat
  , latHemisphere lat
  , lonDecimalMinutes lon
  , lonHemisphere lon
  , show $ fromEnum quality
  , "08"
  , printf "%.1f" (elh /~ metre)
  , "M"
  , "0.0"
  , "M"
  , ""
  , ""
  ]

latHemisphere x = hemisphere "N" "S" (x /~ degree)
lonHemisphere x = hemisphere "E" "W" (x /~ degree)

hemisphere :: (Num b, Ord b) => a -> a -> b -> a
hemisphere pos neg x | x > 0     = pos
                     | otherwise = neg

latDecimalMinutes = decimalMinutes 2 6
lonDecimalMinutes = decimalMinutes 3 6

decimalMinutes :: Int -> Int -> Angle' -> String
decimalMinutes degreeDigits minuteDecimals angle =
  printf format degrees minutes
  where angle' = abs angle
        degrees = truncate (angle' /~ degree)
        minutes = remainder /~ arcminute
        remainder = angle' - (fromInteger degrees *~ degree)
        format = "%0" ++ dWidth ++ "d" ++
                 "%0" ++ mWidth ++ "." ++ mPrec ++ "f"
        dWidth = show degreeDigits
        mWidth = show (minuteDecimals P.+ 3)
        mPrec = show minuteDecimals

nmea :: [String] -> String
nmea fields = "$" ++ sentence ++ "*" ++ checksum
  where sentence = intercalate "," fields
        checksum = nmeaChecksum sentence

nmeaChecksum :: String -> String
nmeaChecksum xs = printf "%2X" $ foldl' xor 0 (map ord xs)
