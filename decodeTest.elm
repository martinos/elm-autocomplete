module Main (..) where

import Html exposing (..)
import ElmTest exposing (..)
import Json.Decode as JD exposing ((:=))
import Decode exposing (..)


tests : Test
tests =
  suite
    "main"
    [ test "should decode the full response"
        <| assertSuccess (JD.decodeString decoder toParse)
    , test "should decode type"
        <| assertSuccess (JD.decodeString typeDecoder typeJson)
    , test "should decode age"
        <| assertSuccess (JD.decodeString ageDecoder ageJson)
    , test "should decode properties"
        <| assertSuccess (JD.decodeString propertiesDecoder properties)
    ]


typeJson =
  """
{"type": "string"}
"""


ageJson =
  """
{ "description": "Age in years",
  "type": "integer",
  "minimum": 0
}
"""


properties =
  """
{
   "firstName": {
     "type": "string"
   },
   "lastName": {
     "type": "string"
   },
   "age": {
     "description": "Age in years",
     "type": "integer",
     "minimum": 0
   }
}
"""


toParse =
  """
{
  "title": "Example Schema",
  "type": "object",
  "properties": {
    "firstName": {
      "type": "string"
    },
    "lastName": {
      "type": "string"
    },
    "age": {
      "description": "Age in years",
      "type": "integer",
      "minimum": 0
    }
  },
  "required": ["firstName", "lastName"]
}
"""


main =
  elementRunner tests



-- Helpers


assertSuccess : Result a b -> Assertion
assertSuccess result =
  case result |> Debug.log "Parsed" of
    Ok _ ->
      assert True

    Err _ ->
      assert False

