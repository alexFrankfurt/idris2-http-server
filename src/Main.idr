module Main

import Network.HTTP.Request
import Network.HTTP.Server
import Network.HTTP.Server.Client
import Data.ByteString
import Data.Buffer.Indexed


s : ByteString
s = fromString "foo\r\nbar"


main : IO ()
main =
  case crlfBreak s of
    Just (p, s) => do
      putStrLn $ toString p
      putStrLn $ toString s
    Nothing => putStrLn "no match"
