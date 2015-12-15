import Html exposing (..)
import Html.Attributes exposing (..) 
import Debug exposing (..)
import ElmTest exposing (..)
import Helper exposing (change)
import Array exposing (..)

type Action
  = Prev
  | Next

find: (a -> Bool) -> List a -> Maybe a
find cond list =
  case list of
    [] -> Nothing
    (x::xs) -> if cond x 
               then Just x
               else find cond xs

withIndex =
  Array.indexedMap (,) 

updateWithIds: Action -> Array (Selectable a)-> Array (Selectable a)
updateWithIds action items =
  let 
    current = items |> withIndex |> Array.filter (snd >> .selected) |> Array.get 0
    size = Array.length items 
  in
    case action of
      Prev -> 
        case current of 
          Nothing -> 
            items |> change (size - 1) select 
          Just (index, item) -> 
            let
              nextElem = items |> Array.get (index - 1)
            in
              case nextElem of
                Nothing -> items
                Just a ->
                  items |> resetSelections |> change (index - 1) select 
      Next -> 
        case current of 
          Nothing -> 
            items |> change 0 select 
          Just (index, item) -> 
            let
              nextElem = items |> Array.get (index + 1)
            in
              case nextElem of
                Nothing -> items
                Just a ->
                  items |> resetSelections |> change (index + 1) select 

update: Action -> List (Selectable a) -> List (Selectable a) 
update action list =
  list |> fromList |> updateWithIds action |> toList

resetSelections  =
  Array.map unselect 

unselect elem = 
  { elem | selected = False }
  
select elem = 
  { elem | selected = True }

toggle elem =
  { elem | selected =  not elem.selected}

type alias Selectable a =
    { a | selected : Bool}

-- Test

testList = [ { name = "Martin", selected = True }
           , { name = "Joe", selected = False} ]

tests = 
  suite "update"
    [ suite "Next"
        [ test "it selects the next element" ( assert ( update Next testList 
                                                        |> List.member { name = "Joe"
                                                                       , selected = True}))
        , test "it deselect the old element" ( assert ( update Next testList 
                                                     |> List.member { name = "Martin"
                                                                    , selected = False}))]]

-- main = renderList list
main = elementRunner tests
