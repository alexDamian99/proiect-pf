import System.IO
import Network.Socket
import Control.Concurrent

main :: IO ()
main = do --withSocketsDo is used for windows ?
    sock <- socket AF_INET Stream 0 --create tcp socket
    setSocketOption sock ReuseAddr 1 -- reuse socket
    bind sock (SockAddrInet 2020 0) -- bind the address and port 2020
    listen sock 10 --maximum 10 connections in queue
    connectionHandler sock -- handle an incoming connection
    close sock -- after socketHandle is don close the main server socket

connectionHandler :: Socket -> IO ()
connectionHandler sock = do
    (connection, _) <- accept sock --accept the conection and get a new socket
    forkIO (clientThread connection) --create a thread for each connection
    connectionHandler sock

clientThread :: Socket -> IO () 
clientThread sock = do
    socketHandler <- socketToHandle sock ReadWriteMode
    hSetBuffering socketHandler NoBuffering --remove buffering
    hPutStrLn socketHandler "Hello!"
    hClose socketHandler
