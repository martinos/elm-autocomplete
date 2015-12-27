import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp 

model = "This is a test"

view address model = 
  div [] 
      [ input [ onChange address Change ] []
      , text model]


type Action
  = Change String

update action model = 
  case action of
    Change text -> text

main = StartApp.start { model = model, view = view, update = update }

onChange address f =
  on "change" targetValue (\v -> Signal.message address (f v))
