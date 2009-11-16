module Time where

import Primitives

import Data.Time
import Data.Ratio ((%))
import Control.Monad (liftM)
import Control.Parallel.Strategies
import Numeric.Units.Dimensional.Prelude

addTime :: Time' -> UTCTime -> UTCTime
addTime x t = addUTCTime (toNominalDiffTime x) t

toNominalDiffTime :: Time' -> NominalDiffTime
toNominalDiffTime t = fromRational $ (ps % 1000000000000)
  where ps = round (t /~ pico second)

toMicroseconds :: Time' -> Int
toMicroseconds t = round (t /~ micro second)

toNearestSecond :: UTCTime -> UTCTime
toNearestSecond utc = utc { utctDayTime = rounded }
  where rounded = secondsToDiffTime seconds
        seconds = round (toRational dayTime)
        dayTime = utctDayTime utc

getRoundedTime :: IO UTCTime
getRoundedTime = liftM toNearestSecond getCurrentTime

instance NFData UTCTime where
  rnf x = (utctDay x) `seq` (utctDayTime x) `seq` ()
