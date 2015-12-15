import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Debug
import Regex

-- example of drop downs http://www.programmableweb.com/category/all/apis?keyword=units%20measurement

type alias Model = { input: String, matches: List Selection, names: List String, key: Int }

type State = Selected | None
type alias Selection = {text: String, state: State}

initModel: Model
initModel = { input = "Content", matches = [], names = toMatch, key = 0 }

toMatch = [ "Martin", "Genevieve", "joe" ]

view: Signal.Address Action -> Model -> Html
view address model =
  div []
      [ input [ type' "text"
              , onInput address UpdateInput
              , value model.input
              , onKeyDown address KeyPress ]
              []
      , text model.input
      , matchesElem model.matches ]
      
matchesElem matches =
  ul []
     [ li []
          ( List.map match matches ) ]
          
match res =
  li [] [text res.text]

type Action
  = NoOp
  | UpdateInput String
  | KeyPress Int

update: Action -> Model -> Model
update action model =
  case action of
  NoOp -> model
  UpdateInput text ->
    let regex = Regex.regex text |> Regex.caseInsensitive
    in
      { model | input = text
              , matches = ( model.names |> 
                            List.filter (Regex.contains regex) 
                            |> List.map (\n -> Selection n None)) }
-- 40 down, 38 up 
  KeyPress mykey -> 
    { model | key = mykey} |> Debug.log "Key pressed"

app = Signal.mailbox NoOp

-- main = view (initModel |> update NoOp)
main = app.signal |> Signal.foldp update initModel |> Signal.map (view app.address)


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
    on "input" targetValue (\v -> Signal.message address (f v))

onChange address f =
  on "change" targetValue (\v -> Signal.message address (f v))
