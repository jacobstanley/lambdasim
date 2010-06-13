{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import System

site :: Snap ()
site = ifTop (fileServe "static/index.html")
   <|> fileServe "static"

main :: IO ()
main = do
    port <- liftM parseArgs getArgs
    putStrLn $ "Starting LambdaλSim on port " ++ show port
    httpServe "*" port "lambdasim"
        (Just "access.log")
        (Just "error.log")
        site


type Port = Int

parseArgs :: [String] -> Port
parseArgs []    = 8080
parseArgs (p:_) = read p
