import Network.Socket hiding (listen)


type HandlerFunc = SockAddr -> String -> IO ()

main = listen "8000" textHandler

listen :: String -> HandlerFunc -> IO ()
listen port handler = withSocketsDo $ do
    serverAddr <- getServerAddr port
    let family = addrFamily serverAddr
    let address = addrAddress serverAddr
  
    putStrLn $ "Listening on " ++ show address ++ " (" ++ show family ++ ")"

    sock <- socket family Datagram defaultProtocol
    bindSocket sock address
    procMessages sock handler

getServerAddr :: String -> IO AddrInfo
getServerAddr port = do
    addrInfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
    return $ head addrInfos

procMessages :: Socket -> HandlerFunc -> IO ()
procMessages sock handler = do
    (msg, _, addr) <- recvFrom sock 1024
    handler addr msg
    procMessages sock handler

textHandler :: HandlerFunc
textHandler addr msg =
    putStrLn $ show addr ++ "> " ++ msg
