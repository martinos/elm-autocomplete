import Html exposing (..)
import StartApp.Simple as StartApp
import AutoComplete exposing (..)

-- example of drop downs http://www.programmableweb.com/category/all/apis?keyword=units%20measurement
qties = [ "tasse", "cuiller a the", "cuiller a soupe" ]
ingredients = [ "farine", "sel", "piments", "bleuet" ]

type alias Model = { unit: AutoComplete.Model, ingredients: AutoComplete.Model }

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
      {model| unit = AutoComplete.update act model.unit} 
    Ingr act ->
      {model| ingredients = AutoComplete.update act model.ingredients} 

type Action
  = NoOp
  | Unit AutoComplete.Action
  | Ingr AutoComplete.Action

view: Signal.Address Action -> Model -> Html
view address model =
  div [] 
       [ AutoComplete.view (Signal.forwardTo address Unit) model.unit 
       , AutoComplete.view (Signal.forwardTo address Ingr) model.ingredients ]

