{-# LANGUAGE TypeOperators #-}

module Lambdasim.Label (
    getL,
    setL,
    headL,
    unitL,
    module Data.Record.Label,
) where

import           Data.Record.Label hiding (get, set)
import qualified Data.Record.Label as L
import           Numeric.Units.Dimensional


getL :: (f :-> a) -> f -> a
getL = L.get

setL :: (f :-> a) -> a -> f -> f
setL = L.set

headL :: [a] :-> a
headL = label getter setter where
    getter          = head
    setter _ []     = []
    setter x (_:xs) = x:xs

unitL :: (Num a, Fractional a) => Unit d a -> (Quantity d a :-> a)
unitL unit = label getter setter where
    getter x   = x /~ unit
    setter x _ = x *~ unit
