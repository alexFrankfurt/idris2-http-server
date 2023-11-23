module Auth.HTTP.Users

import Auth.Users
import Data.Buffer.Indexed
import Data.ByteString
import Data.HTTP.Endpoint
import Data.HTTP.Router
import Data.IORef
import JSON
import Network.HTTP.Application
import Network.HTTP.Headers
import Network.HTTP.Response


usersPath : Path -> Path
usersPath sub = "api" </> "users" </> sub


userPath : Path -> Path
userPath sub = usersPath $ CaptureSegment "id" (Just . MkUserId) sub


usersHandler : Users -> Application
usersHandler users = endpointHandler $ do
  get $ getUsers users
  post $ createUsers users


userHandler : Users -> UserId -> Application
userHandler users userId = endpointHandler $ do
  get $ getUser users userId
  put $ putUser users userId
  delete $ deleteUser users userId


usersRoute : Users -> Route $ usersPath RootPath
usersRoute users = Handler $ usersHandler users


userRoute : Users -> Route $ userPath RootPath
userRoute users = Handler $ userHandler users


public export
usersRouter : Users -> Router
usersRouter users = RoutePath (usersRoute users) <||> RoutePath (userRoute users)
