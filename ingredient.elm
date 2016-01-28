module Ingredient (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp
import AutoComplete exposing (..)
import Group exposing (..)
import Helper
import Maybe


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


emptyModel : Model
emptyModel =
  emptyGroup
    |> add { defaultAutocomplete | choices = ingredients }
    |> add { defaultAutocomplete | choices = qties }
    |> add defaultAutocomplete


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Update id action ->
      model |> Group.changeAt id (AutoComplete.update action)


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
    tr
      []
      (Group.indexedMap widgetElem model)

