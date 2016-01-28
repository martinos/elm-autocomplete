module Group (..) where

import Html exposing (..)
import Graphics.Element exposing (..)
import String


type alias ID =
  Int


type Group a
  = Group (List ( ID, a )) ID


emptyGroup : Group a
emptyGroup =
  Group [] 0


fromList : List a -> Group a
fromList list =
  List.foldl add emptyGroup list


add : a -> Group a -> Group a
add a (Group list id) =
  Group (( id, a ) :: list) (id + 1)


changeAt : ID -> (a -> a) -> Group a -> Group a
changeAt toChange f  (Group list nextID) =
  let
    changeIf ( i, item ) =
      if i == toChange then
        ( i, f item )
      else
        ( i, item )
  in
    Group (List.map changeIf list) nextID


deleteAt : ID -> Group a -> Group a
deleteAt a (Group list nextID) =
  Group (List.filter ((/=) a << fst) list) nextID


indexedMap : (ID -> a -> b) -> Group a -> List b
indexedMap f (Group list nextID) =
  List.map (\( id, elem ) -> f id elem) list

