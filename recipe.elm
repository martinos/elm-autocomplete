import Html exposing (..)
import StartApp.Simple as StartApp
import AutoComplete exposing (..)

-- example of drop downs http://www.programmableweb.com/category/all/apis?keyword=units%20measurement
qties = [ "once", "tasse", "cuiller a the", "cuiller a soupe" ]
ingredients = [ "farine", "sel", "piment", "bleuet", "poivre", "pomme de terre", "pomme verte" ]

type alias Model = { qty: AutoComplete.Model
                   , unit: AutoComplete.Model
                   , ingredients: AutoComplete.Model }


defaultAutocomplete = 
  { input = ""
  , matches = []
  , names = [] 
  , submitted = False }

model: Model
model = { qty  = defaultAutocomplete
        , unit = { defaultAutocomplete | names = qties }
        , ingredients = { defaultAutocomplete | names = ingredients }}

main =
    StartApp.start { model = model, view = view, update = update }

update: Action -> Model -> Model
update action model =
  case action of
    NoOp -> 
      model
    Qty act ->
      {model| qty = AutoComplete.update act model.qty} 
    Unit act ->
      {model| unit = AutoComplete.update act model.unit} 
    Ingr act ->
      {model| ingredients = AutoComplete.update act model.ingredients} 

type Action
  = NoOp
  | Qty AutoComplete.Action
  | Unit AutoComplete.Action
  | Ingr AutoComplete.Action

view: Signal.Address Action -> Model -> Html
view address model =
  div [] 
       [ AutoComplete.view (Signal.forwardTo address Qty) model.qty 
       , AutoComplete.view (Signal.forwardTo address Unit) model.unit 
       , AutoComplete.view (Signal.forwardTo address Ingr) model.ingredients ]

