module Main (..) where

import Html exposing (..)
import StartApp.Simple as StartApp
import AutoComplete exposing (..)
import Helper
import Maybe


-- example of drop downs http://www.programmableweb.com/category/all/apis?keyword=units%20measurement


qties =
  [ "oz"
  , "cup"
  , "tea spoon"
  , "table spoon"
  ]


ingredients =
  [ "flour"
  , "salt"
  , "pepper"
  , "blue berry"
  , "potato"
  , "apple"
  ]


type alias Model =
  List AutoComplete.Model


defaultAutocomplete =
  { input = ""
  , matches = []
  , names = []
  , submitted = False
  }


model : Model
model =
  [ defaultAutocomplete
  , { defaultAutocomplete | names = qties }
  , { defaultAutocomplete | names = ingredients }
  ]


main =
  StartApp.start { model = model, view = view, update = update }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Update id actiono ->
      model
        |> List.indexedMap
            (\i x ->
              if i == id then
                AutoComplete.update actiono x
              else
                x
            )


type Action
  = NoOp
  | Update Int AutoComplete.Action


view : Signal.Address Action -> Model -> Html
view address model =
  let
    widgetAddr id = Signal.forwardTo address (Update id)

    widgetElem id wid = AutoComplete.view (widgetAddr id) wid
  in
    div
      []
      (List.indexedMap widgetElem model)

