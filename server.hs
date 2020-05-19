import Control.Monad.Fix (fix)
import System.IO
import Network.Socket
import Control.Concurrent


type Msg = (String, String)

main :: IO ()
main = do 
    channel <- newChan 
    sock <- socket AF_INET Stream 0 --create tcp socket
    setSocketOption sock ReuseAddr 1 -- reuse socket
    bind sock (SockAddrInet 2020 0) -- bind the address and port 2020
    listen sock 10 --maximum 10 connections in queue
    connectionHandler sock channel -- handle an incoming connection
    close sock -- after socketHandle is don close the main server socket

connectionHandler :: Socket -> Chan Msg -> IO ()
connectionHandler sock channel = do
    putStrLn "Waiting for clients..."
    (connection, _) <- accept sock --accept the conection and get a new socket
    putStrLn "Connected with a client!"
    forkIO (clientThread connection channel) --create a thread for each connection
    connectionHandler sock channel

clientThread :: Socket -> Chan Msg -> IO () 
clientThread sock channel = do
    socketHandler <- socketToHandle sock ReadWriteMode
    hSetBuffering socketHandler NoBuffering --remove buffering

    hPutStrLn socketHandler "Hello client! Please tell me your name"
    name <- (hGetLine socketHandler)

    hPutStrLn socketHandler ("Hello " ++ name ++ ". Now you can exit or create/join a room")
    response <- (hGetLine socketHandler)

    case response of
        "exit" -> do 
                hPutStrLn socketHandler "close"
                hClose socketHandler
        _ -> do
            putStrLn ("[Client " ++ name ++ "]" ++ response)
            hPutStrLn socketHandler "ok"

    communicationChannel <- dupChan channel --creeaza un canal, folosit la citirea informatiilor care vin pe canalul initial (comunicare intre threaduri)
    readingThread <- forkIO $ fix $ \loop -> do
        (senderName, msg) <- readChan communicationChannel
        if (senderName /= name) then do
            hPutStrLn socketHandler (senderName ++ ":" ++ msg)
            loop
        else loop

    putStrLn ("[Client " ++ name ++ "] Moving to client listening and writing")

    fix $ \loop -> do
        message <- fmap init (hGetLine socketHandler)
        putStrLn ("[" ++ name ++ "] " ++ message)
        case message of
            "exit" -> do
                    writeChan channel (name, " left")
            _ -> do
                writeChan channel (name, message) --trimite mesajul tuturor
                loop
    killThread readingThread
