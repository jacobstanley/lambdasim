{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Applicative
import Control.Monad
import "monads-fd" Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import Data.Data
import Data.Typeable
import Prelude hiding (error)
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import System

import qualified Data.ByteString.Char8 as B

import Lambdasim.Snap


main :: IO ()
main = do
    port <- liftM parseArgs getArgs
    putStrLn $ "Starting Lambdaλsim on port " ++ show port
    httpServe "*" port "lambdasim"
        (Just "access.log")
        (Just "error.log")
        site

type Port = Int

parseArgs :: [String] -> Port
parseArgs []    = 8080
parseArgs (p:_) = read p


site :: Snap ()
site = route [ ("",                    fileServe "static/index.html")
             , ("vessel/speed/:speed", method PUT putSpeed)
             , ("vessel/speed/:speed", method GET putSpeed)
             , ("echo/:s",             echo)
             ]
   <|> fileServe "static"


putSpeed :: Snap ()
putSpeed = do
    speed <- paramDouble "speed"
    liftIO $ putStrLn $ show speed
    writeJSON $ speed

echo :: Snap ()
echo = do
    s <- param "s"
    writeJSON $ Echo s

data Echo = Echo {
    eText :: ByteString
} deriving (Eq, Show, Typeable, Data)
