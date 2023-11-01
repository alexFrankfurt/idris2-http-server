module Network.HTTP.Request

import Data.Buffer.Indexed
import Data.ByteString
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.Socket


public export
data Body : Type where
  BodyReader : Socket -> ByteString -> Body


public export
Show Body where
  show (BodyReader _ _) = "<BodyReader>"


public export
record Request where
  constructor MkRequest
  method : Method
  resource : String
  version : String
  headers : Headers
  body : Body


public export
Show Request where
  show req =
    let
      method = show req.method
      resource = show req.resource
      version = show req.version
      headers = show req.headers
      body = show req.body
    in
      "MkRequest { method = " ++ method
                 ++ ", resource = " ++ resource
                 ++ ", version = " ++ version
                 ++ ", headers = " ++ headers
                 ++ ", body = " ++ body ++ " }"


public export
readRequestBody : Body -> IO (Either SocketError ByteString)
readRequestBody (BodyReader sock buf) = do
  Right buf' <- recvByteString 4096 sock
  | Left err => pure $ Left err
  if ByteString.length buf' == 0
    then pure $ Right $ buf `append` buf'
    else readRequestBody (BodyReader sock (buf `append` buf'))
