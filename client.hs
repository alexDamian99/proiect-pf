import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- import qualified Data.ByteString.Char8 as B8
import Control.Concurrent
import System.IO
import Control.Monad.Fix (fix)

main :: IO ()
main = client "127.0.0.1" 2020

client :: String -> Int -> IO ()
client host port = do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                sockH <- socketToHandle sock ReadWriteMode
                msgSender sockH
                close sock

msgSender :: Handle -> IO ()
msgSender socketHandler = do
    hSetBuffering socketHandler NoBuffering

    server_response <- (hGetLine socketHandler) --get the hello from server
    putStrLn server_response

    msg <- getLine
    hPutStrLn socketHandler msg
    server_response <- (hGetLine socketHandler)
    putStrLn server_response

    msg <- getLine  --citeste optiunea
    hPutStrLn socketHandler msg
    server_response <- (hGetLine socketHandler)

    
    
    case server_response of
        "close" -> do
                putStrLn "Closing connection. Bye"
        _ -> do
            listeningThread <- forkIO $ fix $ \loop -> do
                                    msg <- hGetLine socketHandler
                                    putStrLn msg
                                    loop
            fix $ \loop -> do
                msg <- getLine
                hPutStrLn socketHandler (msg ++ " ")
                case msg of
                    "exit" -> do
                        putStrLn "Leaving chat. Bye!"
                    _ -> loop
            
            killThread listeningThread