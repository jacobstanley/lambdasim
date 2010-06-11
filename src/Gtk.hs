module Gtk where

import Control.Monad
import Graphics.UI.Gtk

onInterval :: Int -> IO () -> IO HandlerId
onInterval ms f = timeoutAdd (f >> return True) ms
