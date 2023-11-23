module Auth.Groups

import public Auth.Types
import Data.IORef
import Derive.Prelude
import JSON
import JSON.Derive


%language ElabReflection


public export
record Group where
  constructor MkGroup
  id : GroupId

%runElab derive "Group" [Show, Eq, ToJSON, FromJSON]


public export
record GroupList where
  constructor MkGroupList
  groups : List Group

%runElab derive "GroupList" [Show, Eq, ToJSON, FromJSON]


public export
Groups : Type
Groups = IORef GroupList


public export
newGroups : IO Groups
newGroups = newIORef $ MkGroupList []


public export
createGroups : Groups -> GroupList -> IO GroupList
createGroups groups (MkGroupList gs') = do
  MkGroupList gs <- readIORef groups
  writeAndReturn $ MkGroupList $ replaceGroups gs gs'
  where
    replaceGroups : List Group -> List Group -> List Group
    replaceGroups gs [] = gs
    replaceGroups gs (g' :: gs') =
      replaceGroups (g' :: filter (\g => g.id /= g'.id) gs) gs'
    writeAndReturn : GroupList -> IO GroupList
    writeAndReturn gs = do
      writeIORef groups gs
      pure gs


public export
deleteGroup : Groups -> GroupId -> IO (Maybe Group)
deleteGroup groups gid = do
  MkGroupList gs <- readIORef groups
  writeIORef groups (MkGroupList $ filter (\g => g.id /= gid) gs)
  pure $ find (\g => g.id == gid) gs


public export
getGroup : Groups -> GroupId -> IO (Maybe Group)
getGroup groups gid = do
  MkGroupList gs <- readIORef groups
  pure $ find (\g => g.id == gid) gs


public export
getGroups : Groups -> IO (Maybe GroupList)
getGroups groups = Just <$> readIORef groups


public export
putGroup : Groups -> GroupId -> Group -> IO Group
putGroup groups gid g = do
  MkGroupList gs <- readIORef groups
  writeIORef groups $ MkGroupList $ g :: filter (\g' => g'.id /= gid) gs
  pure g
