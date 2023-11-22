module Data.Users

import Data.IORef
import Derive.Prelude
import JSON
import JSON.Derive


%language ElabReflection


public export
data UserId : Type where
  MkUserId : String -> UserId

%runElab derive "UserId" [Show, Eq, ToJSON, FromJSON]


public export
record User where
  constructor MkUser
  id : UserId
  name : String

%runElab derive "User" [Show, Eq, ToJSON, FromJSON]


public export
record UserList where
  constructor MkUserList
  users : List User

%runElab derive "UserList" [Show, Eq, ToJSON, FromJSON]


public export
deleteUser : IORef (List User) -> UserId -> IO (Maybe User)
deleteUser usersRef id = do
  users <- readIORef usersRef
  let maybeUser = List.head' $ List.filter (\user => User.id user == id) users
  writeIORef usersRef (List.filter (\user => User.id user /= id) users)
  pure maybeUser


public export
getUser : IORef (List User) -> UserId -> IO (Maybe User)
getUser users id = do
  users <- readIORef users
  pure $ List.head' $ List.filter (\user => User.id user == id) users


public export
getUsers : IORef (List User) -> IO UserList
getUsers usersRef = do
  users <- readIORef usersRef
  pure $ MkUserList users


public export
putUser : IORef (List User) -> UserId -> User -> IO User
putUser usersRef userId user = do
  users <- readIORef usersRef
  writeIORef usersRef (user' :: List.filter (\u => u.id /= userId) users)
  pure user'
  where user' = MkUser userId user.name
