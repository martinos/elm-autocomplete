module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D 
import ViewHelper exposing (foundation, onInput)
import StartApp exposing (..)
import Effects
import Task exposing (..)


type alias User =
  { name : String, age : String }


defaultUser : User
defaultUser =
  { name = "Joe", age = "12" }


main =
  app.html


app =
  StartApp.start
    { init = ( defaultUser, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


type Action
  = NoOp
  | InputAge String
  | InputName String
  | Submit


update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    InputName input ->
      ( { model | name = input }, Effects.none )

    InputAge input ->
      ( { model | age = input }, Effects.none )

    Submit ->
      ( model, Effects.none )





view address model =
  div
    []
    [ foundation
    , div
        [ class "row small-8 columns" ]
        [ userForm address model ]
    , div [] [ text <| toString model ]
    ]


userForm address model =
  Html.form
    []
    [ fieldset
        []
        [ legend [] [ text "CONTACT INFO" ]
        , div
            []
            [ label
                []
                [ text "Name"
                , input
                    [ value model.name, type' "text", onInput address InputName ]
                    []
                ]
            ]
        , div
            []
            [ label
                []
                [ text "Age"
                , input
                    [ value model.age, type' "number", onInput address InputAge ]
                    []
                ]
            ]
        , a [ class "small button", onClick address Submit ] [ text "Submit" ]
        ]
    ]

