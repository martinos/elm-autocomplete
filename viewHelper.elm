module ViewHelper (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


css : String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []


foundation =
  css "http://cdn.foundation5.zurb.com/foundation.css"


debug : Attribute
debug =
  style [ ( "background-color", "red" ) ]


onChange : Signal.Address a -> (String -> a) -> Attribute
onChange address f =
  on
    "change"
    targetValue
    (\v -> Signal.message address (f v))


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

