module Network.HTTP.Server

import Data.ByteString
import Data.IORef
import Network.HTTP.Application
import Network.HTTP.Connection
import Network.HTTP.Headers
import Network.HTTP.Request
import Network.HTTP.Response
import Network.Socket
import System.Info


data ServerError : Type where
  ServerBindError : Int -> ServerError
  ServerListenError : Int -> ServerError
  ServerSocketError : SocketError -> ServerError


export
Show ServerError where
  show (ServerBindError err) = "Bind error: " ++ show err
  show (ServerListenError err) = "Listen error: " ++ show err
  show (ServerSocketError err) = "Socket error: " ++ show err


listenOn : Port -> IO (Either ServerError Socket)
listenOn port = do
  Right sock <- socket AF_INET Stream 0
  | Left err => pure $ Left $ ServerSocketError err
  bindResult <- bind sock Nothing port
  if bindResult /= 0
    then pure $ Left $ ServerBindError bindResult
    else do
      listenResult <- listen sock
      if listenResult /= 0
         then pure $ Left $ ServerListenError listenResult
         else pure $ Right sock


serverConnectionHandler : Connection -> SocketAddress -> Application -> IO ()
serverConnectionHandler connection addr app = do
  putStrLn "[server] handling connection"
  MkConnectionBuffer _ sock <- readIORef connection
  readResult <- readRequestHeaders connection
  case readResult of
    Left err => do
      putStrLn $ "[server] readRequestHeaders failed: " ++ show err
      -- putStrLn "[server] terminating connection after read failure"
      -- close sock
      pure ()
    Right request => do
      putStrLn $ "[server] received request: " ++ show request
      appResult <- app request $ mkRespond sock
      case appResult of
        SendResponseError _ err => do
          putStrLn $ "[server] send response failed: " ++ show err
          putStrLn "[server] terminating connection after send failure"
          close sock
        SentResponse response => do
          putStrLn $ "[server] sent response: " ++ show response.status
          serverConnectionHandler connection addr app


closeClientSocket : Socket -> IO ()
closeClientSocket sock =
  if os == "windows"
     then putStrLn "[server] client socket close skipped on Windows"
     else do
       close sock
       putStrLn "[server] closed client socket"


serverConnectionAcceptor : Socket -> Application -> IO ()
serverConnectionAcceptor serverSock app = do
  -- Accept the connection
  Right (clientSock, clientAddr) <- accept serverSock
  | Left err => do
      putStrLn $ "[server] accept failed: " ++ show err
      pure ()
  putStrLn $ "[server] accepted connection from: " ++ show clientAddr
  putStrLn "[server] about to spawn handler"
  _ <- fork $ do
    putStrLn "[server] fork entered"
    putStrLn "[server] spawned handler"
    connection <- newConnection clientSock
    putStrLn "[server] created new connection"
    serverConnectionHandler connection clientAddr app
    putStrLn "[server] handler finished"
    closeClientSocket clientSock
  putStrLn "[server] fork returned"
  pure ()



serverLoop : Socket -> Application -> IO ServerError
serverLoop serverSock app = do
  -- Handle the next connection
  serverConnectionAcceptor serverSock app
  -- Loop to the next connection
  serverLoop serverSock app


export
listenAndServe : Port -> Application -> IO ServerError
listenAndServe port app = do
  Right sock <- listenOn port
  | Left err => pure err
  serverLoop sock app
