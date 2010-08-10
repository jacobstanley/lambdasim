{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import"monads-fd"Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           System
import           Text.JSON

import           Prelude ()
import           Lambdasim.Prelude
import           Lambdasim.Console
import           Lambdasim.Label hiding (label)
import           Lambdasim.Snap
import           Lambdasim.SimRunner
import           Lambdasim.Simulation
import           Lambdasim.STM
import           Network.Info


main :: IO ()
main = do
    enableUTF8
    port <- liftM parseArgs getArgs
    sim <- startSimulation
    putStrLn $ "Starting Lambda\x03bbsim on port " ++ show port
    httpServe "*" port "hostname"
        (Just "access.log")
        (Just "error.log")
        (site sim)

type Port = Int

parseArgs :: [String] -> Port
parseArgs []    = 8080
parseArgs (p:_) = read p


site :: TVar Simulation -> Snap ()
site sim = catch500 $ route
         [ putV "speed"   speed   knot
         , putV "heading" heading degree
         , putV "rudder"  rudder (degree / second)
         , get "vessel" $ getSim sim
         , get "network" $ networkInfo
         ]
       <|> ifTop (fileServe "static/index.html")
       <|> fileServe "static"
  where
    putV = putVessel sim

putVessel :: TVar Simulation -> ByteString -> (Vessel :-> Quantity d) -> Unit d -> Route
putVessel sim param label unit = put url $ modify param label' sim
  where
    url = B.concat ["vessel/", param, "/:", param]
    label' = unitL unit . label . headL . vessels 

modify :: ByteString -> (a :-> Double) -> TVar a -> Snap ()
modify param label tvar = do
    raw <- paramDouble param
    liftIO $ stmUpdate_ (setL label raw) tvar
    liftIO $ putStrLn $ B.unpack param ++ ": " ++ show raw
    modifyResponse $ setContentType "application/json"

getSim :: TVar Simulation -> Snap ()
getSim sim = do
    s <- liftIO $ stmRead sim
    writeJSON $ toJson $ head $ getL vessels s
  where
    toJson v = makeObj' [("speed",   getL speed   v /~ knot),
                         ("heading", getL heading v /~ degree),
                         ("rudder",  getL rudder  v /~ (degree / second))
                        ]

networkInfo :: Snap ()
networkInfo = do
    networks <- liftIO getIPs
    writeJSON $ toJSObject [("networks", networks)]
  where
    getIPs   = liftM extract getNetworkInterfaces
    extract  = map show . filter (/= nullIPv4) . map ipv4
    nullIPv4 = IPv4 0

makeObj' :: [(String, Double)] -> JSValue
makeObj' xs = makeObj $ map f xs
  where
    f (k, v) = (k, JSRational True $ toRational v)
