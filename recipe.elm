module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp
import AutoComplete exposing (..)
import Group exposing (..)
import Helper
import Maybe


-- example of drop downs http://www.programmableweb.com/category/all/apis?keyword=units%20measurement


qties =
  [ "oz"
  , "cup"
  , "tea spoon"
  , "table spoon"
  , "unit"
  ]


ingredients =
  [ "flour"
  , "salt"
  , "pepper"
  , "blue berry"
  , "banana"
  , "potato"
  , "apple"
  ]


type alias Model =
  Group AutoComplete.Model


model : Model
model =
  emptyGroup
  |> add { defaultAutocomplete | choices = ingredients }
  |> add { defaultAutocomplete | choices = qties }
  |> add defaultAutocomplete
  


main =
  StartApp.start { model = model, view = view, update = update }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Update id action ->
      model |> Group.changeAt (AutoComplete.update action) id

type Action
  = NoOp
  | Update Int AutoComplete.Action


view : Signal.Address Action -> Model -> Html
view address model =
  let
    widgetAddr id = Signal.forwardTo address (Update id)

    widgetElem id wid =
      td [] [ AutoComplete.view (widgetAddr id) wid ]
  in
    div
      []
      [ table
          []
          [ tr [] (Group.indexedMap widgetElem model) ]
      ]

