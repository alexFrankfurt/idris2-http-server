module Auth.Types

import Derive.Prelude
import JSON
import JSON.Derive


%language ElabReflection


public export
data GroupId : Type where
  MkGroupId : String -> GroupId

%runElab derive "GroupId" [Show, Eq, ToJSON, FromJSON]


public export
data UserId : Type where
  MkUserId : String -> UserId

%runElab derive "UserId" [Show, Eq, ToJSON, FromJSON]
