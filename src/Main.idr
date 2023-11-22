module Main

import Data.Buffer.Indexed
import Data.ByteString
import Data.List1
import Data.String
import Data.HTTP.Router
import Network.HTTP.Application
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Request
import Network.HTTP.Response
import Network.HTTP.Server
import Network.Socket.Data


data UserId : Type where
  MkUserId : String -> UserId


usersPath : Path -> Path
usersPath sub = "api" </> "users" </> sub


userPath : Path -> Path
userPath sub = usersPath $ CaptureSegment "id" (Just . MkUserId) sub


usersHandler : Application
usersHandler request respond =
  respond $ MkResponse statusOK empty "Hello, users!\n"


userHandler : UserId -> Application
userHandler (MkUserId id) request respond =
  respond $ MkResponse statusOK empty $ "Hello, user!\n"


usersRoute : Route (usersPath RootPath)
usersRoute = Handler usersHandler


userRoute : Route (userPath RootPath)
userRoute = Handler userHandler


appRoute : Router
appRoute = RoutePath usersRoute <||> RoutePath userRoute


handler : Application
handler = route appRoute


main : IO ()
main = do
  result <- listenAndServe 8080 handler
  putStrLn $ "Error: " ++ show result
