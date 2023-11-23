module Auth.Users

import public Auth.Types
import Data.IORef
import Derive.Prelude
import JSON
import JSON.Derive


public export
record User where
  constructor MkUser
  id : UserId
  email : String

%runElab derive "User" [Show, Eq, ToJSON, FromJSON]


public export
record UserList where
  constructor MkUserList
  users : List User

%runElab derive "UserList" [Show, Eq, ToJSON, FromJSON]


public export
Users : Type
Users = IORef (List User)


public export
newUsers : IO Users
newUsers = newIORef []


public export
createUsers : Users -> UserList -> IO UserList
createUsers usersRef (MkUserList users') = do
  users <- readIORef usersRef
  writeAndReturn $ replaceUsers users users'
  where
    replaceUsers : List User -> List User -> List User
    replaceUsers users [] = users
    replaceUsers users (user :: users') =
      replaceUsers (user :: filter (\u => u.id /= user.id) users) users'
    writeAndReturn : List User -> IO UserList
    writeAndReturn users = do
      writeIORef usersRef users
      pure $ MkUserList users


public export
deleteUser : Users -> UserId -> IO (Maybe User)
deleteUser usersRef id = do
  users <- readIORef usersRef
  let maybeUser = List.head' $ List.filter (\user => User.id user == id) users
  writeIORef usersRef (List.filter (\user => User.id user /= id) users)
  pure maybeUser


public export
getUser : Users -> UserId -> IO (Maybe User)
getUser users id = do
  users <- readIORef users
  pure $ List.head' $ List.filter (\user => user.id == id) users


public export
getUsers : Users -> IO (Maybe UserList)
getUsers usersRef = Just <$> MkUserList <$> readIORef usersRef


public export
putUser : Users -> UserId -> User -> IO User
putUser usersRef userId user = do
  users <- readIORef usersRef
  writeIORef usersRef $ user :: filter (\u => u.id /= userId) users
  pure user
