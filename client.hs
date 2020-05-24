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

    msg <- getLine --get name
    hPutStrLn socketHandler msg
    server_response <- (hGetLine socketHandler)
    putStrLn server_response

    
    fix $ \main_loop -> do 
        putStrLn "Enter exit/create/join"
        msg <- getLine  --citeste optiunea
        hPutStrLn socketHandler msg
        server_response <- (hGetLine socketHandler)
        case server_response of
            "close" -> do
                putStrLn "Closing connection. Bye"
            "created" -> do
                server_response <- (hGetLine socketHandler)
                putStrLn ("Created room " ++ server_response)
                communication socketHandler
            "room?" -> do
                putStrLn "Enter a room id (number):"
                msg <- getLine
                hPutStrLn socketHandler msg
                server_response <- (hGetLine socketHandler)
                case server_response of 
                    "joined" -> do
                        roomNo <- (hGetLine socketHandler)
                        putStrLn ("Joined room " ++ roomNo)
                        communication socketHandler
                    "err" -> do
                        server_response <- (hGetLine socketHandler)
                        putStrLn ("Error " ++ server_response)
                        main_loop
                



communication :: Handle -> IO ()
communication socketHandler = do
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