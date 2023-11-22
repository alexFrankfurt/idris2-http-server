module Network.HTTP.Connection

import Data.Buffer.Indexed
import Data.ByteString
import Network.HTTP.Headers
import Network.HTTP.Methods
import Network.HTTP.Protocol
import Network.Socket


public export
record HTTPConnection where
  constructor Connection
  buffer : ByteString
  socket : Socket

export
Show HTTPConnection where
  show (Connection _ _) = "<HTTPConnection>"


public export
data HTTPConnectionError : Type where
  ConnectionSocketError : SocketError -> HTTPConnectionError
  ConnectionProtocolError : ProtocolError -> HTTPConnectionError


public export
Show HTTPConnectionError where
  show (ConnectionSocketError err) = "ConnectionSocketError: " ++ show err
  show (ConnectionProtocolError err) = "ConnectionProtocolError: " ++ show err


crlfBreak : ByteString -> Maybe (ByteString, ByteString)
crlfBreak bs = match 0 False $ uncons bs where
  match : Nat -> Bool -> Maybe (Bits8, ByteString) -> Maybe (ByteString, ByteString)
  match _ _ Nothing = Nothing
  match n False (Just (13, bs')) = match (n + 1) True $ uncons bs'
  match (S n) True (Just (10, bs')) = Just (take n bs, bs')
  match n _ (Just (_, bs')) = match (n + 1) False $ uncons bs'


export
recvBytes : HTTPConnection -> Nat -> IO (Either SocketError (ByteString, HTTPConnection))
recvBytes (Connection buf sock) n = do
  Nothing <- pure $ splitAt n buf
  | Just (bs, buf') => pure $ Right (bs, Connection buf' sock)
  Right bs <- recvByteString n sock
  | Left err => pure $ Left err
  recvBytes (Connection (buf `append` bs) sock) n


export
recvLine : HTTPConnection -> IO (Either SocketError (ByteString, HTTPConnection))
recvLine (Connection buf sock) = do
  Nothing <- pure $ crlfBreak buf
  | Just (line, buf') => pure $ Right (line, Connection buf' sock)
  Right bs <- recvByteString 4096 sock
  | Left err => pure $ Left err
  recvLine $ Connection (buf `append` bs) sock


export
recvHeaders : HTTPConnection
           -> Headers
           -> IO (Either HTTPConnectionError (Headers, HTTPConnection))
recvHeaders client0 headers = do
  Right (line, client1) <- recvLine client0
  | Left err => pure $ Left $ ConnectionSocketError err
  Right (Just header) <- pure $ parseHeader $ toString line
  | Right Nothing => pure $ Right (headers, client1)
  | Left err => pure $ Left $ ConnectionProtocolError err
  recvHeaders client1 (addHeader header headers)
