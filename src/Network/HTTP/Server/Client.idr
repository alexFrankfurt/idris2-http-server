module Network.HTTP.Server.Client

import Data.Buffer.Indexed
import Data.ByteString
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Protocol
import Network.HTTP.Request
import Network.Socket


public export
data HTTPClient : Type where
  Client : (buf : ByteString) -> (sock : Socket) -> HTTPClient


public export
data HTTPClientError : Type where
  ClientSocketError : SocketError -> HTTPClientError
  ClientProtocolError : ProtocolError -> HTTPClientError


public export
Show HTTPClientError where
  show (ClientSocketError err) = "ClientSocketError: " ++ show err
  show (ClientProtocolError err) = "ClientProtocolError: " ++ show err


crlfBreak : ByteString -> Maybe (ByteString, ByteString)
crlfBreak bs = match 0 False $ uncons bs where
  match : Nat -> Bool -> Maybe (Bits8, ByteString) -> Maybe (ByteString, ByteString)
  match _ _ Nothing = Nothing
  match n False (Just (13, bs')) = match (n + 1) True $ uncons bs'
  match (S n) True (Just (10, bs')) = Just (take n bs, bs')
  match n _ (Just (_, bs')) = match (n + 1) False $ uncons bs'


recvBytes : HTTPClient -> Nat -> IO (Either SocketError (ByteString, HTTPClient))
recvBytes (Client buf sock) n = do
  Nothing <- pure $ splitAt n buf
  | Just (bs, buf') => pure $ Right (bs, Client buf' sock)
  Right bs <- recvByteString n sock
  | Left err => pure $ Left err
  recvBytes (Client (buf `append` bs) sock) n


recvLine : HTTPClient -> IO (Either SocketError (ByteString, HTTPClient))
recvLine (Client buf sock) = do
  Nothing <- pure $ crlfBreak buf
  | Just (line, buf') => pure $ Right (line, Client buf' sock)
  Right bs <- recvByteString 4096 sock
  | Left err => pure $ Left err
  recvLine $ Client (buf `append` bs) sock


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
  Right (line, client) <- recvLine $ Client buf sock
  | Left err => pure $ Left $ ClientSocketError err
  Just (method, resource, version) <- pure $ parseRequestLine $ toString line
  | _ => pure $ Left $ ClientProtocolError $ ProtocolErrorMessage "Invalid request"
  Right (headers, Client buf' sock') <- recvHeaders client empty
  | Left err => pure $ Left err
  Just method' <- pure $ stringToMethod method
  | Nothing => pure $ Left $ ClientProtocolError $ ProtocolErrorMessage "Invalid method"
  pure $ Right $ MkRequest method' resource version headers $ BodyReader buf' sock'
