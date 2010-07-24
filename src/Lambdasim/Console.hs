{-# LANGUAGE CPP #-}

module Lambdasim.Console (
    enableUTF8
) where

#ifdef WINDOWS
import System.Win32.Console (setConsoleCP, setConsoleOutputCP)
#endif

-- | Enables the UTF8 code page on Windows
enableUTF8 :: IO ()
#ifdef WINDOWS
enableUTF8 = do
    setConsoleCP utf8
    setConsoleOutputCP utf8
  where
    utf8 = 65001
#else
enableUTF8 = return ()
#endif
