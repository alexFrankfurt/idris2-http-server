module Data.HTTP.Users

import Data.Buffer.Indexed
import Data.ByteString
import Data.HTTP.Endpoint
import Data.HTTP.Router
import Data.IORef
import Data.Users
import JSON
import Network.HTTP.Application
import Network.HTTP.Headers
import Network.HTTP.Response


usersPath : Path -> Path
usersPath sub = "api" </> "users" </> sub


userPath : Path -> Path
userPath sub = usersPath $ CaptureSegment "id" (Just . MkUserId) sub


usersHandler : IORef (List User) -> Application
usersHandler users = endpointHandler $ do
  get $ Just <$> getUsers users


userHandler : IORef (List User) -> UserId -> Application
userHandler users userId = endpointHandler $ do
  get $ getUser users userId
  put $ putUser users userId
  delete $ deleteUser users userId


usersRoute : IORef (List User) -> Route $ usersPath RootPath
usersRoute users = Handler $ usersHandler users


userRoute : IORef (List User) -> Route $ userPath RootPath
userRoute users = Handler $ userHandler users


public export
usersRoutes : IORef (List User) -> Router
usersRoutes users = RoutePath (usersRoute users) <||> RoutePath (userRoute users)
