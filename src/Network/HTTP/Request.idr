module Network.HTTP.Request

import Data.Buffer.Indexed
import Data.ByteString
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.Socket


public export
record Body where
  constructor BodyReader
  buffer : ByteString
  socket : Socket


public export
Show Body where
  show _ = "<BodyReader>"


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
readRequestBody : Request -> IO (Either SocketError ByteString)
readRequestBody request = do
  Right buffer <- recvByteString 4096 request.body.socket
  | Left err => pure $ Left err
  let buffer' = request.body.buffer `append` buffer
  if ByteString.length buffer' == 0
    then pure $ Right buffer'
    else readRequestBody $ { body := { buffer := buffer' } request.body } request
