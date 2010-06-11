{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Main where

import System
import Control.Applicative
import "monads-fd" Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import Text.Templating.Heist
import qualified Data.ByteString.Char8 as B
import qualified Text.XML.Expat.Tree as X

template :: TemplateState Snap -> ByteString -> Snap ()
template state name = do
    maybe pass writeBS =<< renderTemplate state name
    modifyResponse $ setContentType "text/html"

site :: TemplateState Snap -> Snap ()
site ts =
    ifTop (fileServe "static/index.html") <|>
    fileServe "static"

timeSplice :: Splice Snap
timeSplice = do
    time <- liftIO getCurrentTime
    return $ [X.Text $ B.pack $ show time]

loadError :: String -> String
loadError str = "Error loading templates\n"++str

main :: IO ()
main = do
    args <- getArgs

    let port = case args of
                   []  -> 8080
                   p:_ -> read p

    (origTs, _) <- bindStaticTag .
                   bindSplice "time" timeSplice
                   $ emptyTemplateState

    eitherTs <- loadTemplates "templates" origTs
    let ts = either error id eitherTs

    either (\s -> putStrLn (loadError s) >> exitFailure)
           (const $ return ())
           eitherTs
    
    putStrLn $ "Starting LambdaλSim on port " ++ show port

    httpServe "*" port "lambdasim"
        (Just "access.log")
        (Just "error.log")
        (site ts)
