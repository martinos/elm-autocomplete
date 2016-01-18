module AutoComplete (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex
import Selector
import Helper exposing (..)
import String exposing (isEmpty)


type alias Model =
  { input : String
  , matches : List Selection
  , names : List String
  , submitted : Bool
  }


type alias Selection =
  { text : String, selected : Bool }


view : Signal.Address Action -> Model -> Html
view address model =
  if model.submitted then
    showElem model
  else
    editElem address model


editElem : Signal.Address Action -> Model -> Html
editElem address model =
  div
    [ style [ ( "float", "left" ) ] ]
    [ input
        [ type' "text"
        , onInput address UpdateInput
        , onChange address SubmitInput
        , value model.input
        , onArrow address arrowToAction
        ]
        []
    , matchesElem address model.matches
    ]


showElem : Model -> Html
showElem model =
  div
    [ style [ ( "float", "left" ) ] ]
    [ text model.input ]


matchesElem address matches =
  ul
    []
    (List.indexedMap (match address) matches)


selectedStyles =
  [ ( "background-color", "blue" )
  , ( "color", "white" )
  , ( "list-style-type", "none" )
  ]


selectionStyle =
  [ ( "list-style-type", "none" ) ]


match : Signal.Address Action -> Int -> Selection -> Html
match address id res =
  let
    style' =
      if res.selected then
        selectedStyles ++ selectionStyle
      else
        selectionStyle
  in
    li
      [ style style'
      , onMouseOver address (Select id)
      ]
      [ text res.text ]



-- Update


type Action
  = NoOp
  | UpdateInput String
  | SubmitInput String
  | Up
  | Down
  | Select Int


update : Action -> Model -> Model
update action model =
  let
    updateMatches f model =
      { model | matches = model.matches |> f }
  in
    case action of
      NoOp ->
        model

      UpdateInput text ->
        let
          regex = text |> Regex.escape |> Regex.regex |> Regex.caseInsensitive
        in
          if String.isEmpty text then
            { model
              | input = text
              , matches = []
            }
          else
            { model
              | input = text
              , matches =
                  (model.names
                    |> List.filter (Regex.contains regex)
                    |> List.map (\n -> Selection n False)
                  )
            }
              |> updateMatches (Selector.update (Selector.Select 0))

      SubmitInput text ->
        let
          selected = model.matches |> List.filter .selected |> List.head
        in
          case selected of
            Nothing ->
              { model | submitted = True }

            Just elem ->
              { model | input = elem.text, submitted = True }

      Down ->
        model |> updateMatches (Selector.update Selector.Next)

      Up ->
        model |> updateMatches (Selector.update Selector.Prev)

      Select id ->
        model |> updateMatches (Selector.update (Selector.Select id))


arrowToAction k =
  case k of
    38 ->
      Up

    40 ->
      Down

    _ ->
      NoOp


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))


onChange address f =
  on "change" targetValue (\v -> Signal.message address (f v))


app =
  Signal.mailbox NoOp



-- Test app


initModel : Model
initModel =
  { input = ""
  , matches = []
  , names = toMatch
  , submitted = False
  }


toMatch =
  [ "ml", "once", "tasse", "cuiller a the", "cuiller a soupe" ]


main =
  app.signal
    |> Signal.foldp update initModel
    |> Signal.map (view app.address)

