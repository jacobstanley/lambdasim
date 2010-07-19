{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lambdasim.Snap where

import           Control.Applicative
import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import"monads-fd"Control.Monad.Trans ()
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lex.Double (readDouble)
import           Data.Data
import qualified Data.Text as T
import           Prelude hiding (catch)
import           Snap.Types
import           Text.JSON.Generic
import qualified Text.XHtmlCombinators.Escape as XH


writeJSON :: (JSON a) => a -> Snap ()
writeJSON json = do
    writeBS $ B.pack $ encode json
    modifyResponse $ setContentType "application/json"


type Route = (ByteString, Snap ())
type MethodRoute = ByteString -> Snap () -> Route

methodRoute :: Method -> MethodRoute
methodRoute m r h = (r, method m h)

get :: MethodRoute
get = methodRoute GET

put :: MethodRoute
put = methodRoute PUT

post :: MethodRoute
post = methodRoute POST

delete :: MethodRoute
delete = methodRoute DELETE


paramString :: ByteString -> Snap ByteString
paramString = paramMap Just

paramDouble :: ByteString -> Snap Double
paramDouble = paramMap $ (fst <$>) . readDouble

paramMap :: (ByteString -> Maybe a) -> ByteString -> Snap a
paramMap f name = getParam name >>= \mstr -> case mstr of
    Nothing  -> throwEx ["parameter '", name, "' does not exist"]
    Just str -> case f str of
        Nothing  -> throwEx ["'", str, "' is not a valid value for ",
                             "parameter '", name, "'"]
        Just val -> return val
  where
    throwEx xs = throw $ ParamException $ B.unpack $ B.concat xs


data ParamException = ParamException String
    deriving (Typeable)

instance Exception ParamException

instance Show ParamException where
    show (ParamException msg) = "ParamException: " ++ msg


catch500 :: Snap a -> Snap ()
catch500 m = (m >> return ()) `catch` \(e::SomeException) -> do
    let t = T.pack $ show e
    putResponse r
    writeBS "<html><head><title>Internal Server Error</title></head>"
    writeBS "<body><h1>Internal Server Error</h1>"
    writeBS "<p>A web handler threw an exception. Details:</p>"
    writeBS "<pre>\n"
    writeText $ XH.escape t
    writeBS "\n</pre></body></html>"

    logError $ B.append "Error: " $ B.pack $ show e
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse
