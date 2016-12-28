module Main (..) where

import Html exposing (..)
import ViewHelper exposing (..)
import StartApp exposing (..)
import Effects exposing (..)
import Task exposing (..)
import Http
import Json.Decode as JD exposing ((:=))


type alias Model =
  List Range


type alias Range =
  { minDigits : Int, maxDigits : Int }


dummyRange : Range
dummyRange =
  { minDigits = 5143334444, maxDigits = 5143336666 }


app =
  StartApp.start
    { init = ( [ dummyRange ], getRanges )
    , update = update
    , view = view
    , inputs = []
    }


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


main =
  app.html


type Action
  = NoOp
  | NewRanges (Result Http.Error (List Range))


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action |> Debug.log "Action" of
    NoOp ->
      ( model, Effects.none )

    NewRanges rangesResult ->
      case rangesResult of
        Ok ranges ->
          ( ranges, Effects.none )

        Err error ->
          ( model, Effects.none )


getRanges =
  get' decoder "http://api:c34359ea348f1026f90a3cafd7c4a096@martport.isptelecom.net/tel_num_orders/customer_dids.json?npa_nxx=514312&qty=10"
    |> Task.toResult
    |> Task.map NewRanges
    |> Effects.task


get' : JD.Decoder value -> String -> Task Http.Error value
get' decoder url =
  let request =
        { verb = "GET"
        , headers = [("Authorization", "Basic YXBpOmMzNDM1OWVhMzQ4ZjEwMjZmOTBhM2NhZmQ3YzRhMDk2")]
        , url = url
        , body = Http.empty
        }
  in
      Http.fromJson decoder (Http.send Http.defaultSettings request)
url =
  Http.url "http://api:c34359ea348f1026f90a3cafd7c4a096@martport.isptelecom.net/tel_num_orders/customer_dids.json?npa_nxx=514312&qty=10"

decoder =
  JD.list
    (JD.object2
      Range
      ("min" := JD.int)
      ("max" := JD.int)
    )



-- View


view address model =
  div
    []
    [ foundation
    , tnTable model
    ]


tnTable tns =
  table
    []
    [ header
    , tbody
        []
        (List.map tnRow tns)
    ]


header =
  thead
    []
    [ tr
        []
        [ th [] [ text "Min Digits" ]
        , th [] [ text "Max Digits" ]
        ]
    ]


tnRow range =
  tr
    []
    [ td [] [ text <| toString range.minDigits ]
    , td [] [ text <| toString range.maxDigits ]
    ]

