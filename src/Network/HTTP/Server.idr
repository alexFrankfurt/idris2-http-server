module Network.HTTP.Server

import Data.ByteString
import Data.IORef
import Network.HTTP.Application
import Network.HTTP.Connection
import Network.HTTP.Headers
import Network.HTTP.Request
import Network.HTTP.Response
import Network.Socket


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
  -- Receive the request
  Right request <- readRequestHeaders connection
  | Left err => do
      putStrLn $ "[server] readRequestHeaders failed: " ++ show err
      pure ()
  putStrLn $ "[server] received request: " ++ show request
  -- Invoke the app to send the response
  SentResponse response <- app request $ mkRespond sock
  | SendResponseError _ err => do
      putStrLn $ "[server] send response failed: " ++ show err
      pure ()
  putStrLn $ "[server] sent response: " ++ show response.status
  -- Handle the next request
  serverConnectionHandler connection addr app


serverConnectionAcceptor : Socket -> Application -> IO ()
serverConnectionAcceptor serverSock app = do
  -- Accept the connection
  Right (clientSock, clientAddr) <- accept serverSock
  | Left err => do
      putStrLn $ "[server] accept failed: " ++ show err
      pure ()
  putStrLn $ "[server] accepted connection from: " ++ show clientAddr
  -- Handle the connection synchronously for now
  connection <- newConnection clientSock
  putStrLn "[server] created new connection"
  serverConnectionHandler connection clientAddr app
  close clientSock
  putStrLn "[server] closed client socket"
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
