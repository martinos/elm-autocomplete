module AutoComplete (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex
import Group exposing (..)
import Selector exposing (..)
import Helper exposing (..)
import String exposing (isEmpty)


type alias Model =
  { input : String
  , matches : Selector String
  , names : List String
  , submitted : Bool
  }


defaultAutocomplete =
  { input = ""
  , matches = emptySelector
  , names = []
  , submitted = False
  }


view : Signal.Address Action -> Model -> Html
view address model =
  if model.submitted then
    showHtml model
  else
    editHtml address model


editHtml : Signal.Address Action -> Model -> Html
editHtml address model =
  div
    []
    [ input
        [ type' "text"
        , onInput address UpdateInput
        , onChange address SubmitInput
        , value model.input
        , onArrow address arrowToAction
        ]
        []
    , matchesHtml address model.matches
    ]


showHtml : Model -> Html
showHtml model =
  div
    [ style [ ( "float", "left" ) ] ]
    [ text model.input ]


matchesHtml : Signal.Address Action -> Selector String -> Html
matchesHtml address matches =
  ul
    [ style
        [ ( "margin", "0px" )
        , ( "padding", "0px" )
        ]
    ]
    (Group.indexedMap
      (matchHtml address (cursor matches))
      (group matches)
    )


selectedStyles =
  [ ( "background-color", "blue" )
  , ( "color", "white" )
  , ( "list-style-type", "none" )
  ]


selectionStyle =
  [ ( "list-style-type", "none" ) ]


matchHtml : Signal.Address Action -> Maybe ID -> ID -> String -> Html
matchHtml address selection id res =
  let
    style' =
      if selection == Just id then
        selectedStyles ++ selectionStyle
      else
        selectionStyle
  in
    li
      [ style style'
      , onMouseOver address (Select id)
      ]
      [ text res ]



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
            { model | input = text, matches = emptySelector }
          else
            { model
              | input = text
              , matches =
                  Selector Nothing (List.foldl add emptyGroup model.names) |> next
            }

      SubmitInput text ->
        let
          selected = selection model.matches
        in
          case selected of
            Nothing ->
              { model | submitted = True }

            Just elem ->
              { model | input = elem, submitted = True }

      Down ->
        model |> updateMatches next

      Up ->
        model |> updateMatches prev

      Select id ->
        model


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
  , matches = emptySelector
  , names = toMatch
  , submitted = False
  }


toMatch =
  [ "ml", "once", "tasse", "cuiller a the", "cuiller a soupe" ]


main =
  app.signal
    |> Signal.foldp update initModel
    |> Signal.map (view app.address)

