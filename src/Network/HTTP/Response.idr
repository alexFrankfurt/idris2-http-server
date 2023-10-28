module Network.HTTP.Response

import Data.Buffer.Indexed
import Data.ByteString
import Network.HTTP.Headers
import Network.HTTP.Request


public export
record Response where
  constructor MkResponse
  status : Int
  headers : Headers
  body : ByteString


public export
Show Response where
  show response
    = "Response "
    ++ show response.status ++ " "
    ++ show response.headers ++ " "
    ++ show response.body


export
statusString : Int -> String
statusString 100 = "Continue"
statusString 101 = "Switching Protocols"
statusString 200 = "OK"
statusString 201 = "Created"
statusString 202 = "Accepted"
statusString 203 = "Non-Authoritative Information"
statusString 204 = "No Content"
statusString 205 = "Reset Content"
statusString 206 = "Partial Content"
statusString 300 = "Multiple Choices"
statusString 301 = "Moved Permanently"
statusString 302 = "Found"
statusString 303 = "See Other"
statusString 304 = "Not Modified"
statusString 305 = "Use Proxy"
statusString 307 = "Temporary Redirect"
statusString 400 = "Bad Request"
statusString 401 = "Unauthorized"
statusString 402 = "Payment Required"
statusString 403 = "Forbidden"
statusString 404 = "Not Found"
statusString 405 = "Method Not Allowed"
statusString 406 = "Not Acceptable"
statusString 407 = "Proxy Authentication Required"
statusString 408 = "Request Time-out"
statusString 409 = "Conflict"
statusString 410 = "Gone"
statusString 411 = "Length Required"
statusString 412 = "Precondition Failed"
statusString 413 = "Request Entity Too Large"
statusString 414 = "Request-URI Too Large"
statusString 415 = "Unsupported Media Type"
statusString 416 = "Requested range not satisfiable"
statusString 417 = "Expectation Failed"
statusString 418 = "I'm a teapot"
statusString 500 = "Internal Server Error"
statusString 501 = "Not Implemented"
statusString 502 = "Bad Gateway"
statusString 503 = "Service Unavailable"
statusString 504 = "Gateway Time-out"
statusString 505 = "HTTP Version not supported"
statusString _ = "Unknown Status Code"


export
responseLine : Request -> Response -> String
responseLine request response =
  request.version ++ " " ++ show response.status ++ " " ++ statusString response.status


export
responseHeaders : Headers -> String
responseHeaders [] = ""
responseHeaders ((MkHeader k []) :: headers) = responseHeaders headers
responseHeaders ((MkHeader k (v::vs)) :: headers) =
  k ++ ": " ++ v ++ "\r\n" ++ (responseHeaders ((MkHeader k vs) :: headers))


export
responseString : Request -> Response -> String
responseString request response
  = responseLine request response ++ "\r\n"
  ++ responseHeaders response.headers ++ "\r\n"
  ++ toString response.body


export
withContentLength : Response -> Response
withContentLength response =
  if hasHeader "Content-Length" response.headers
     then response
     else response'
  where
    header = MkHeader "Content-Length" [show (length response.body)]
    headers' = header :: response.headers
    response' = MkResponse response.status headers' response.body
