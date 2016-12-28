module Decode (..) where

import Json.Decode exposing (..)


type alias Record =
  { title : String
  , type' : String
  , properties : Properties
  , required : List String
  }


decoder =
  succeed Record
    |> apply ("title" := string)
    |> apply ("type" := string)
    |> apply ("properties" := propertiesDecoder)
    |> apply ("required" := (list string))


type alias Type =
  { type' : String }


typeDecoder =
  succeed Type
    |> apply ("type" := string)


type alias Age =
  { description : String
  , type' : String
  , minimum : Int
  }


ageDecoder =
  succeed Age
    |> apply ("description" := string)
    |> apply ("type" := string)
    |> apply ("minimum" := int)


type alias Properties =
  { firstName : Type
  , lastName : Type
  , age : Age
  }


propertiesDecoder =
  succeed Properties
    |> apply ("firstName" := typeDecoder)
    |> apply ("lastName" := typeDecoder)
    |> apply ("age" := ageDecoder)


apply value func =
  object2 (<|) func value

