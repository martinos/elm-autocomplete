import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Debug
import Regex
import Selector
import Json.Decode exposing (customDecoder)

-- example of drop downs http://www.programmableweb.com/category/all/apis?keyword=units%20measurement

type alias Model = { input: String, matches: List Selection, names: List String, key: Int }
type alias Selection = {text: String, selected: Bool}

initModel: Model
initModel = { input = "", matches = [], names = toMatch, key = 0 }

toMatch = [ "Martin", "Mario", "Genevieve", "joe" ]

view: Signal.Address Action -> Model -> Html
view address model =
  div []
      [ input [ type' "text"
              , onInput address UpdateInput
              , value model.input
              , onArrow address]
              []
      , matchesElem model.matches ]
      
matchesElem matches =
  ul []
     [ li []
          ( List.map match matches ) ]
          
match res =
  let
    elem = if res.selected then
      strong [] [text res.text]
    else
      text res.text
  in
    li [] [elem]

type Action
  = NoOp
  | UpdateInput String
  | Up
  | Down

update: Action -> Model -> Model
update action model =
  let 
    updateMatches f model =
      { model | matches = model.matches |> f }
  in
    case action of
    NoOp -> model
    UpdateInput text ->
      let regex = Regex.regex text |> Regex.caseInsensitive
      in
        { model | input = text
                , matches = ( model.names 
                              |> List.filter (Regex.contains regex) 
                              |> List.map (\n -> Selection n False)) }
    Down -> model |> updateMatches (Selector.update Selector.Next) 
    Up-> model |> updateMatches (Selector.update Selector.Prev)

onArrow addr =
  onWithOptions "keydown" {preventDefault = True, stopPropagation = False}
  (customDecoder keyCode isArrow)
  (\k ->
      Signal.message addr <|
      case k of
        38 -> Up
        40 -> Down 
        _ -> NoOp)
isArrow =
  (\k ->
    if k == 40 || k == 38
    then Ok k
    else Err "not arrow")
app = Signal.mailbox NoOp

-- main = view (initModel |> update NoOp)
main = app.signal |> Signal.foldp update initModel |> Signal.map (view app.address)

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
    on "input" targetValue (\v -> Signal.message address (f v))

onChange address f =
  on "change" targetValue (\v -> Signal.message address (f v))
