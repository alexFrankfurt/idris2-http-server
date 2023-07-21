module Network.HTTP.Server.Client

import Data.Buffer.Indexed
import Data.ByteString
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Protocol
import Network.HTTP.Request
import Network.Socket


export
data HTTPClient : Type where
  Client : (buf : ByteString) -> (sock : Socket) -> HTTPClient


export
data HTTPClientError : Type where
  ClientSocketError : SocketError -> HTTPClientError
  ClientProtocolError : ProtocolError -> HTTPClientError


export
crlfBreak : ByteString -> Maybe (ByteString, ByteString)
crlfBreak bs = match (NM empty) bs where
  data MS : Type where
    M : (p : ByteString) -> MS
    NM : (p : ByteString) -> MS
    CR : (p : ByteString) -> MS
  match : MS -> ByteString -> Maybe (ByteString, ByteString)
  match ms s =
    case (ms, uncons s) of
      (M p, _) => Just (p, s)                           -- Matched CRLF
      (NM p, Just (13, s')) => match (CR p) s'          -- Matched CR
      (CR p, Just (10, s')) => match (M p) s'           -- Matched LF
      (NM p, Just (b, s')) => match (NM $ snoc b p) s'  -- No match
      (CR p, Just (b, s')) => match (NM $ snoc b p) s'  -- No match
      (_, Nothing) => Nothing


export
recvBytes : HTTPClient -> Nat -> IO (Either SocketError (ByteString, HTTPClient))
recvBytes (Client buf sock) n = do
  Nothing <- pure $ splitAt n buf
  | Just (bs, buf') => pure $ Right (bs, Client buf' sock)
  Right bs <- recvByteString n sock
  | Left err => pure $ Left err
  recvBytes (Client (buf `append` bs) sock) n


export
recvLine : HTTPClient -> IO (Either SocketError (ByteString, HTTPClient))
recvLine (Client buf sock) = do
  Nothing <- pure $ crlfBreak buf
  | Just (line, buf') => pure $ Right (line, Client buf' sock)
  Right bs <- recvByteString 4096 sock
  | Left err => pure $ Left err
  recvLine $ Client (buf `append` bs) sock


export
recvHeaders : HTTPClient
           -> Headers
           -> IO (Either HTTPClientError (Headers, HTTPClient))
recvHeaders client0 headers = do
  Right (line, client1) <- recvLine client0
  | Left err => pure $ Left $ ClientSocketError err
  Right (Just header) <- pure $ parseHeader $ toString line
  | Right Nothing => pure $ Right (headers, client1)
  | Left err => pure $ Left $ ClientProtocolError err
  recvHeaders client1 (addHeader header headers)


export
recvRequest : HTTPClient -> IO (Either HTTPClientError Request)
recvRequest (Client buf sock) = do
  Right (line, client0) <- recvLine $ Client buf sock
  | Left err => pure $ Left $ ClientSocketError err
  Just (method, resource, "HTTP/1.1") <- pure $ parseRequestLine $ toString line
  | _ => pure $ Left $ ClientProtocolError $ ProtocolErrorMessage "Invalid request"
  Right (headers, client1) <- recvHeaders client0 empty
  | Left err => pure $ Left err
  Just method' <- pure $ stringToMethod method
  | Nothing => pure $ Left $ ClientProtocolError $ ProtocolErrorMessage "Invalid method"
  pure $ Right $ MkRequest method' resource headers ?recvBody
