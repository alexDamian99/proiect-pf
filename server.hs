import Control.Monad.Fix (fix)
import System.IO
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM
import Data.Char

type Msg = (String, Integer, String)

main :: IO ()
main = do 
    channel <- newChan 
    sock <- socket AF_INET Stream 0 --create tcp socket
    setSocketOption sock ReuseAddr 1 -- reuse socket
    bind sock (SockAddrInet 2020 0) -- bind the address and port 2020
    listen sock 1024 --maximum 10 connections in queue

    totalRooms <- newTVarIO (0 :: Int)

    connectionHandler sock channel totalRooms -- handle an incoming connection
    close sock -- after socketHandle is don close the main server socket

connectionHandler :: Socket -> Chan Msg -> TVar Int -> IO ()
connectionHandler sock channel totalRooms = do
    fix $ \loop -> do
        putStrLn "Waiting for clients..."
        (connection, _) <- accept sock --accept the conection and get a new socket
        putStrLn "Connected with a client!"
        forkIO (clientThread connection channel totalRooms) --create a thread for each connection
        loop


clientThread :: Socket -> Chan Msg -> TVar Int -> IO () 
clientThread sock channel totalRooms = do

    socketHandler <- socketToHandle sock ReadWriteMode
    hSetBuffering socketHandler NoBuffering --remove buffering

    hPutStrLn socketHandler "Hello client! Please tell me your name"
    name <- (hGetLine socketHandler)

    hPutStrLn socketHandler ("Hello " ++ name ++ ". Now you can exit or create/join a room")
    
    let roomNo = 0
    fix $ \loop -> do
        response <- (hGetLine socketHandler)
        
        case response of
            "exit" -> do 
                    hPutStrLn socketHandler "close"
                    hClose socketHandler
            "create" -> do 
                    rooms <- atomically $ readTVar totalRooms
                    let roomNo = (fromIntegral rooms)
                    atomically $ modifyTVar totalRooms (+1)
                    hPutStrLn socketHandler "created"
                    hPutStrLn socketHandler (show roomNo)
                    communication socketHandler channel name roomNo
            
            "join" -> do
                    rooms <- atomically $ readTVar totalRooms
                    hPutStrLn socketHandler "room?"
                    roomNo <- (hGetLine socketHandler)
                    putStrLn (show rooms)
                    if((all isDigit roomNo) && ((read roomNo :: Int) <= (fromIntegral rooms))) then do
                            hPutStrLn socketHandler "joined"
                            hPutStrLn socketHandler roomNo
                            communication socketHandler channel name (read roomNo :: Integer)
                    else do
                        hPutStrLn socketHandler "err"
                        hPutStrLn socketHandler "The room id must be a number or the id of a created room"
                        loop
            _ -> do
                putStrLn ("[Client " ++ name ++ "]" ++ response)
                hPutStrLn socketHandler "err"
                hPutStrLn socketHandler "Invalid command"
                loop
        

communication :: Handle -> Chan Msg -> String -> Integer -> IO ()
communication socketHandler channel name roomNo = do
    communicationChannel <- dupChan channel --creeaza un canal, folosit la citirea informatiilor care vin pe canalul initial (comunicare intre threaduri)
    readingThread <- forkIO $ fix $ \loop -> do
        (senderName, roomNumber, msg) <- readChan communicationChannel
        if (senderName /= name && roomNumber == roomNo) then do
            case msg of
                " left" -> hPutStrLn socketHandler (senderName ++ " left the conversation.")
                "join" -> do
                    hPutStrLn socketHandler (senderName ++ " joined the conversation.")
                    loop
                _ -> do
                    hPutStrLn socketHandler (senderName ++ ":" ++ msg)
                    loop
        else loop

    putStrLn ("[Client " ++ name ++ "] Moving to client listening and writing")
    writeChan channel (name, roomNo, "join")
    fix $ \loop -> do
        message <- fmap init (hGetLine socketHandler)
        putStrLn ("[" ++ name ++ "] " ++ message)
        case message of
            "exit" -> do
                    writeChan channel (name, roomNo, " left")
            _ -> do
                writeChan channel (name, roomNo, message) --trimite mesajul tuturor
                loop
    killThread readingThread