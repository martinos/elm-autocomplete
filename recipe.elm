import Html exposing (..)
import Debug
import StartApp.Simple as StartApp
import SelectionBox

-- example of drop downs http://www.programmableweb.com/category/all/apis?keyword=units%20measurement
qties = [ "tasse", "cuiller a the", "cuiller a soupe" ]
ingredients = [ "farine", "sel", "piments", "bleuet" ]

type alias Model = { unit: SelectionBox.Model, ingredients: SelectionBox.Model }

model: Model
model = { unit = { input = ""
                  , matches = []
                  , names = qties
                  , submitted = False }
        , ingredients = { input = ""
                        , matches = []
                        , names = ingredients 
                        , submitted = False}}

main =
    StartApp.start { model = model, view = view, update = update }

update: Action -> Model -> Model
update action model =
  case action of
    NoOp -> 
      model
    Unit act ->
      {model| unit = SelectionBox.update act model.unit} 
    Ingr act ->
      {model| ingredients = SelectionBox.update act model.ingredients} 

type Action
  = NoOp
  | Unit SelectionBox.Action
  | Ingr SelectionBox.Action

view: Signal.Address Action -> Model -> Html
view address model =
  div [] 
       [ SelectionBox.view (Signal.forwardTo address Unit) model.unit 
       , SelectionBox.view (Signal.forwardTo address Ingr) model.ingredients ]

