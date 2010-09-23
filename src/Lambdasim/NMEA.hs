module Lambdasim.NMEA (
    FixQuality(..),
    gga,
) where

import           Data.Bits (xor)
import           Data.Char (ord)
import           Data.List
import           Data.Time.Clock
import           Data.Time.Format (formatTime)
import           System.Locale (defaultTimeLocale)
import           Text.Printf (printf)

import qualified Prelude as P ((+))
import           Lambdasim.Prelude
import           Lambdasim.Geographical


data FixQuality
    = Invalid | GPS | DGPS | PPS  | RTK | FloatRTK
    | DeadReckoning | Manual | Simulation
    deriving (Enum, Show)

gga :: UTCTime -> Geog -> FixQuality -> String
gga time (Geog lat lon elh) quality = nmea
    [ "GPGGA"
    , formatTime defaultTimeLocale "%H%M%S" time
    , latDecimalMinutes lat -- latitude
    , latHemisphere lat     -- latitude hemisphere
    , lonDecimalMinutes lon -- longitude
    , lonHemisphere lon     -- longitude hemisphere
    , show $ fromEnum quality
    , "08"  -- number of satellites being tracked
    , "0.0" -- horizontal dilution of position
    , printf "%.1f" (elh /~ metre) -- altitude, metres above mean sea level
    , "M"   -- units for altitude
    , "0.0" -- height of geoid (mean sea level) above WGS84
    , "M"   -- units for height
    , ""    -- time in seconds since last DGPS update
    , ""    -- DGPS station ID number
    ]

latHemisphere :: Angle -> String
latHemisphere x = hemisphere "N" "S" (x /~ degree)

lonHemisphere :: Angle -> String
lonHemisphere x = hemisphere "E" "W" (x /~ degree)

hemisphere :: (Num b, Ord b) => a -> a -> b -> a
hemisphere pos neg x | x > 0     = pos
                     | otherwise = neg

latDecimalMinutes :: Angle -> String
latDecimalMinutes = decimalMinutes 2 6

lonDecimalMinutes :: Angle -> String
lonDecimalMinutes = decimalMinutes 3 6

decimalMinutes :: Int -> Int -> Angle -> String
decimalMinutes degreeDigits minuteDecimals angle =
    printf format degrees minutes
  where
    angle'    = abs angle
    degrees   = truncate (angle' /~ degree)
    minutes   = remainder /~ arcminute
    remainder = angle' - (fromInteger degrees *~ degree)
    format    = "%0" ++ dWidth ++ "d" ++
                "%0" ++ mWidth ++ "." ++ mPrec ++ "f"
    dWidth    = show degreeDigits
    mWidth    = show (minuteDecimals P.+ 3)
    mPrec     = show minuteDecimals

nmea :: [String] -> String
nmea fields = "$" ++ sentence ++ "*" ++ checksum sentence ++ "\r\n"
  where
    sentence = intercalate "," fields

checksum :: String -> String
checksum xs = printf "%2X" $ foldl' xor 0 (map ord xs)
