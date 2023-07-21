module Network.HTTP.Request

import Network.HTTP.Headers
import Network.HTTP.Methods


public export
data Body : Type where
  BodyPending : HasIO io => io Body -> Body
  BodyString : String -> Body


public export
record Request where
  constructor MkRequest
  method : Method
  resource : String
  headers : Headers
  body : Body
