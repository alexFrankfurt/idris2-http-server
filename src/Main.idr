module Main

import Data.Buffer.Indexed
import Data.ByteString
import Network.HTTP.Application
import Network.HTTP.Headers
import Network.HTTP.Request
import Network.HTTP.Response
import Network.HTTP.Server
import Network.Socket.Data


handler : Application
handler _ respond = respond $ MkResponse 200 empty "Hello, world!"


main : IO ()
main = do
  result <- listenAndServe 8080 handler
  putStrLn $ "Error: " ++ show result
