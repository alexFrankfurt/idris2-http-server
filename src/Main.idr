module Main

import Data.HTTP.Router
import Data.HTTP.Users
import Data.Users
import Data.IORef
import Network.HTTP.Application
import Network.HTTP.Server
import Network.Socket.Data


handler : IORef (List User) -> Application
handler users = route $ usersRoutes users


main : IO ()
main = do
  users <- newIORef []
  result <- listenAndServe 8080 $ handler users
  putStrLn $ "Error: " ++ show result
