module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)


main =
  view { names = [ "Omer", "Marge" ], inputName = "None" }


type alias Model =
  { names : List String
  , inputName : String
  }


debug : Attribute
debug =
  style [ ( "background-color", "red" ) ]


view : Model -> Html
view model =
  div
    []
    [ div [] (valueInput "name" "Omer")
    , ul [] (values model.names)
    ]


valueInput : String -> String -> List Html
valueInput label' value' =
  [ label [] [ text label' ]
  , input [ value value' ] []
  ]


itemHtml : Html
itemHtml =
  nothing


values : List String -> List Html
values =
  List.map value'


value' : String -> Html
value' name =
  li [] [ text name ]


nothing : Html
nothing =
  text ""
