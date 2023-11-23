module Main

import Auth.Groups
import Auth.Users
import Auth.HTTP.Groups
import Auth.HTTP.Users
import Data.IORef
import Data.HTTP.Router
import Network.HTTP.Application
import Network.HTTP.Server
import Network.Socket.Data


handler : Users -> Groups -> Application
handler users groups = route $ usersRouter users <||> groupsRouter groups


main : IO ()
main = do
  users <- newUsers
  groups <- newGroups
  result <- listenAndServe 8080 $ handler users groups
  putStrLn $ "Error: " ++ show result
