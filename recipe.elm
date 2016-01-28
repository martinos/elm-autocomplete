module Main (..) where

import StartApp.Simple as StartApp
import Ingredient as Ing
import Group
import Html exposing (..)


main : Signal Html
main =
  StartApp.start
    { model =
        Group.fromList
          [ Ing.emptyModel
          , Ing.emptyModel
          , Ing.emptyModel
          , Ing.emptyModel
          , Ing.emptyModel
          ]
    , view = view
    , update = update
    }


type alias Model =
  Group.Group Ing.Model


type Action
  = NoOp
  | Update Int Ing.Action


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Update id action' ->
      Group.changeAt id (Ing.update action') model


view : Signal.Address Action -> Model -> Html
view address model =
  let
    widgetAddr id = Signal.forwardTo address (Update id)

    widgetElem id wid =
      Ing.view (widgetAddr id) wid
  in
    table
      []
      (Group.indexedMap widgetElem model)

