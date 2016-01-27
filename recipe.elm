module Main (..) where

import StartApp.Simple as StartApp
import Ingredient as Ing
import AutoComplete
import Group
import Html exposing (..)


main =
  StartApp.start { model = (Group.emptyGroup |> Group.add Ing.model |> Group.add Ing.model), view = view, update = update }


type alias Model
  = Group.Group Ing.Model 


type Action
  = NoOp
  | Update Int Ing.Action


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Update id action' ->
      Group.changeAt (Ing.update action') id model

view : Signal.Address Action -> Model -> Html
view address model =
  let
    widgetAddr id = Signal.forwardTo address (Update id)

    widgetElem id wid =
      Ing.view (widgetAddr id) wid
  in
    div
      []
      [ table
          []
          (Group.indexedMap widgetElem model)
      ]

