module Network.HTTP.Headers

import Data.String
import Network.HTTP.Protocol


export
record Header where
  constructor MkHeader
  name : String
  values : List String


export
Headers : Type
Headers = List Header


export
parseHeader : String -> Either ProtocolError (Maybe Header)
parseHeader line =
  if null line
  then Right Nothing
  else
    let
      (name, rest) = break (== ':') line
      maybeValue =
        case strM rest of
          StrCons ' ' rest' => Just rest'
          _ => Nothing
    in return name maybeValue
  where
    return : String -> Maybe String -> Either ProtocolError (Maybe Header)
    return name (Just value) = Right $ Just $ MkHeader name [value]
    return _ Nothing = Left $ ProtocolErrorMessage "Invalid header"


export
addHeader : Header -> Headers -> Headers
addHeader header [] = [header]
addHeader header (MkHeader name values :: headers) =
  if name == header.name
  then MkHeader name (header.values ++ values) :: headers
  else MkHeader name values :: addHeader header headers


export
empty : Headers
empty = []
