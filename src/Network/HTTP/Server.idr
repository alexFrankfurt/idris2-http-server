module Network.HTTP.Server

import Data.ByteString
import Network.HTTP.Application
import Network.HTTP.Headers
import Network.HTTP.Request
import Network.HTTP.Response
import Network.HTTP.Server.Client
import Network.Socket


data HTTPServerError : Type where
  ServerBindError : Int -> HTTPServerError
  ServerListenError : Int -> HTTPServerError
  ServerSocketError : SocketError -> HTTPServerError


export
Show HTTPServerError where
  show (ServerBindError err) = "Bind error: " ++ show err
  show (ServerListenError err) = "Listen error: " ++ show err
  show (ServerSocketError err) = "Socket error: " ++ show err


listenOn : Port -> IO (Either HTTPServerError Socket)
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


serverClientHandler : Socket -> SocketAddress -> Application -> IO ()
serverClientHandler sock _ app = do
  -- Receive the request
  Right request <- recvRequest $ Client empty sock
  | Left err => putStrLn $ "Receive request failed: " ++ show err
  -- Print the request
  putStrLn $ show request
  -- Invoke the app to send the response
  SentResponse response <- app request $ mkRespond sock request
  | SendResponseError _ err => putStrLn $ "Send response failed: " ++ show err
  -- Print the response
  putStrLn $ show response


serverClientAcceptor : Socket -> Application -> IO ()
serverClientAcceptor serverSock app = do
  -- Accept the connection
  Right (clientSock, clientAddr) <- accept serverSock
  | Left err => putStrLn $ "Accept failed: " ++ show err
  -- Fork the connection handler
  _ <- fork $ do
    serverClientHandler clientSock clientAddr app
    -- Close the connection
    close clientSock
  pure ()


serverLoop : Socket -> Application -> IO HTTPServerError
serverLoop serverSock app = do
  -- Handle the next connection
  serverClientAcceptor serverSock app
  -- Loop to the next connection
  serverLoop serverSock app


export
listenAndServe : Port -> Application -> IO HTTPServerError
listenAndServe port app = do
  Right sock <- listenOn port
  | Left err => pure err
  serverLoop sock app
