module Auth.HTTP.Groups

import Auth.Groups
import Data.HTTP.Endpoint
import Data.HTTP.Router
import JSON
import Network.HTTP.Application


groupsPath : Path -> Path
groupsPath sub = "api" </> "groups" </> sub


groupPath : Path -> Path
groupPath sub = groupsPath $ CaptureSegment "id" (Just . MkGroupId) sub


groupsHandler : Groups -> Application
groupsHandler groups = endpointHandler $ do
  get $ getGroups groups
  post $ createGroups groups


groupHandler : Groups -> GroupId -> Application
groupHandler groups id = endpointHandler $ do
  get $ getGroup groups id
  put $ putGroup groups id
  delete $ deleteGroup groups id


groupsRoute : Groups -> Route $ groupsPath RootPath
groupsRoute groups = Handler $ groupsHandler groups


groupRoute : Groups -> Route $ groupPath RootPath
groupRoute groups = Handler $ groupHandler groups


export
groupsRouter : Groups -> Router
groupsRouter groups = RoutePath (groupsRoute groups) <||> RoutePath (groupRoute groups)
