module Lambdasim.Udp (
    sendMsg,
    testUdp,
) where

import Network.Socket hiding (listen)
import Data.List (genericDrop)
import System.IO (hFlush,stdout)


sendMsg :: String -> String -> String -> IO ()
sendMsg hostname port msg = withSocketsDo $ do
    serverAddr <- getServerAddr hostname port
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    sendStr sock msg (addrAddress serverAddr)


sendStr :: Socket -> String -> SockAddr -> IO ()
sendStr _ [] _ = return ()
sendStr sock msg addr = do
    sent <- sendTo sock msg addr
    --putStrLn ("(Sent " ++ show sent ++ " bytes to " ++ show addr ++ ")")
    sendStr sock (genericDrop sent msg) addr
    sClose sock
  
getServerAddr :: String -> String -> IO AddrInfo
getServerAddr hostname port = do
    addrInfos <- getAddrInfo Nothing (Just hostname) (Just port)
    return $ head addrInfos


testUdp :: IO ()
testUdp = do
    putStr "> "
    hFlush stdout
    msg <- getLine
    sendMsg "127.0.0.1" "8000" msg
    testUdp
