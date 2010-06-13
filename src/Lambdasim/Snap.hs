{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Lambdasim.Snap where

import Control.Applicative
import "monads-fd" Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lex.Double (readDouble)
import Data.Data
import Snap.Types
import Text.JSON.Generic

import qualified Data.ByteString.Char8 as B

writeJSON :: (Data a) => a -> Snap ()
writeJSON json = do
    writeBS $ B.pack $ encodeJSON json
    modifyResponse $ setContentType "application/json"

param :: ByteString -> Snap ByteString
param = paramMap Just

paramDouble :: ByteString -> Snap Double
paramDouble = paramMap $ (fst <$>) . readDouble

paramMap :: (ByteString -> Maybe a) -> ByteString -> Snap a
paramMap f name = getParam name >>= \mstr -> case mstr of
    Nothing  -> snapError ["parameter '", name, "' does not exist"]
    Just str -> case f str of
        Nothing  -> snapError ["'", str, "' is not a valid value for ",
                               "parameter '", name, "'"]
        Just val -> return val

snapError :: [ByteString] -> Snap a
snapError msg = do
    putResponse $ setResponseStatus 500 "Internal Server Error" emptyResponse
    let fullMsg = B.concat $ "Error: " : msg
    writeBS fullMsg
    logError fullMsg
    liftIO $ B.putStrLn fullMsg
    r <- getResponse
    finishWith r
    empty
