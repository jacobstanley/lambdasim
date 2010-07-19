{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import"monads-fd"Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Record.Label as L
import           Numeric.Units.Dimensional.Prelude
import           Prelude ()
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           System
import           Text.JSON

import           Lambdasim.Snap
import           Lambdasim.Primitives
import           Lambdasim.SimRunner
import           Lambdasim.Simulation
import           Lambdasim.STM


main :: IO ()
main = do
    port <- liftM parseArgs getArgs
    sim <- startSimulation
    putStrLn $ "Starting Lambdaλsim on port " ++ show port
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
         [ put "vessel/speed/:speed"     $ putSpeed sim
         , put "vessel/heading/:heading" $ putHeading sim
         , put "vessel/rudder/:rudder"   $ putRudder sim
         , get "vessel"                  $ getSim sim
         ]
       <|> ifTop (fileServe "static/index.html")
       <|> fileServe "static"
  where
    putSpeed   = modifyVessel "speed"   $ \x -> L.set vesSpeed   $ x *~ knot
    putHeading = modifyVessel "heading" $ \x -> L.set vesHeading $ x *~ degree
    putRudder  = modifyVessel "rudder"  $ \x -> L.set vesRudder  $ x *~ (degree / second)

modifyVessel :: ByteString -> (Double -> Vessel -> Vessel) -> TVar Simulation -> Snap ()
modifyVessel paramName f sim = do
    value <- paramDouble paramName
    liftIO $ stmUpdate_ (updateFirstVessel $ f value) sim
    liftIO $ putStrLn $ B.unpack paramName ++ ": " ++ show value
    modifyResponse $ setContentType "application/json"

getSim :: TVar Simulation -> Snap ()
getSim sim = do
    s <- liftIO $ stmRead sim
    writeJSON $ toJson $ head $ L.get simVessels s
  where
    toJson v = makeObj' [("speed",   L.get vesSpeed   v /~ knot),
                         ("heading", L.get vesHeading v /~ degree),
                         ("rudder",  L.get vesRudder  v /~ (degree / second))
                        ]

makeObj' :: [(String, Double)] -> JSValue
makeObj' xs = makeObj $ map f xs
  where
    f (k, v) = (k, JSRational True $ toRational v)
