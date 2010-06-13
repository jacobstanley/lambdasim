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
    st <- newMVar $ SimState 0
    putStrLn $ "Starting Lambdaλsim on port " ++ show port
    httpServe "*" port "hostname"
        (Just "access.log")
        (Just "error.log")
        (site $ st)

type Port = Int

parseArgs :: [String] -> Port
parseArgs []    = 8080
parseArgs (p:_) = read p


data SimState = SimState {
    simSpeed :: Double
} deriving (Data, Typeable)

site :: MVar SimState -> Snap ()
site sim = catch500 $
          route [ ("",                    fileServe "static/index.html")
                , ("vessel/speed/:speed", method PUT $ putSpeed sim)
                , ("vessel",              method GET $ getSim sim)
                , ("echo/:s",             echo)
                ]
      <|> fileServe "static"


putSpeed :: MVar SimState -> Snap ()
putSpeed sim = do
    speed <- paramDouble "speed"
    liftIO $ modifyMVar_ sim $ \x -> return x { simSpeed = speed }

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
