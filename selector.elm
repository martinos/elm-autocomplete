module Selector where
import ElmTest exposing (..)
import Helper exposing (changeAt)
import Array exposing (..)

type Action
  = Prev
  | Next
  | Select Int 
  | NoOp

type alias Selectable a =
    { a | selected : Bool}

update: Action -> List (Selectable a) -> List (Selectable a) 
update action list =
  list |> fromList |> updateWithIds action 
       |> toList

updateWithIds: Action -> Array (Selectable a) -> Array (Selectable a)
updateWithIds action items =
  let 
    index = selectedItemIndex items    
    size = Array.length items
  in
    case action of
      Prev -> 
        items |> navigate prev (size - 1) index
      Next -> 
        items |> navigate next 0 index
      Select id ->
        items |> setSelection id 
      NoOp -> 
        items

selectedItemIndex items = 
  items |> withIndex 
        |> Array.filter (snd >> .selected) 
        |> Array.get 0 
        |> (flip Maybe.andThen) (fst >> Just)

next x = x + 1
prev x = x - 1 

navigate: (Int -> Int) ->  Int -> Maybe Int -> Array (Selectable a) -> Array (Selectable a)
navigate step start_id index items = 
  case index of 
    Nothing -> 
      items |> changeAt start_id select
    Just index -> 
      setSelection (step index) items

setSelection: Int -> Array (Selectable a) -> Array (Selectable a)
setSelection index items =
  let 
    selection = items |> Array.get index
  in
    case selection of
      Nothing -> 
        items 
      Just a ->
        items |> resetSelections |> changeAt index select

resetSelections  =
  Array.map unselect 

unselect elem = 
  { elem | selected = False }
  
select elem = 
  { elem | selected = True }

toggle elem =
  { elem | selected =  not elem.selected}

withIndex =
  Array.indexedMap (,) 

