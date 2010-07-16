{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import"monads-fd"Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Data
import           Data.Typeable
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           System

import           Lambdasim.Snap


main :: IO ()
main = do
    port <- liftM parseArgs getArgs
    sim <- newMVar $ SimState 0 0 0
    putStrLn $ "Starting Lambdaλsim on port " ++ show port
    httpServe "*" port "hostname"
        (Just "access.log")
        (Just "error.log")
        (site sim)

type Port = Int

parseArgs :: [String] -> Port
parseArgs []    = 8080
parseArgs (p:_) = read p


data SimState = SimState {
    simSpeed :: Double,
    simHeading :: Double,
    simRudder :: Double
} deriving (Data, Typeable)


site :: MVar SimState -> Snap ()
site sim = catch500 $
           route
           [ get ""                        $ fileServe "static/index.html"
           , put "vessel/speed/:speed"     $ putSpeed sim
           , put "vessel/heading/:heading" $ putHeading sim
           , put "vessel/rudder/:rudder"   $ putRudder sim
           , get "vessel"                  $ getSim sim
           ]
       <|> fileServe "static"


putSpeed   = modifySim "speed"   $ \p x -> x { simSpeed = p }
putHeading = modifySim "heading" $ \p x -> x { simHeading = p }
putRudder  = modifySim "rudder"  $ \p x -> x { simRudder = p }

modifySim :: ByteString -> (Double -> SimState -> SimState) -> MVar SimState -> Snap ()
modifySim paramName f sim = do
    p <- paramDouble paramName
    liftIO $ modifyMVar_ sim $ \x -> return (f p x)
    liftIO $ putStrLn $ (B.unpack paramName) ++ ": " ++ (show p)
    modifyResponse $ setContentType "application/json"

getSim :: MVar SimState -> Snap ()
getSim sim = do
    s <- liftIO $ readMVar sim
    writeJSON s

echo :: Snap ()
echo = do
    s <- param "s"
    writeJSON $ Echo s

data Echo = Echo {
    eText :: ByteString
} deriving (Eq, Show, Typeable, Data)
