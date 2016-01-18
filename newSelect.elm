module NewSelect (..) where

import Html exposing (..)
import Graphics.Element exposing (..)
import String


type alias ID =
  Int


type Group a
  = Group (List ( ID, a )) ID


listWithId (Group listWithId _) =
  listWithId


emptyGroup : Group a
emptyGroup =
  Group [] 0


add : a -> Group a -> Group a
add a (Group list id) =
  Group (( id, a ) :: list) (id + 1)


changeAt : (a -> a) -> ID -> Group a -> Group a
changeAt f toChange (Group list id) =
  let
    changeIf ( i, item ) =
      if i == toChange then
        ( i, f item )
      else
        ( i, item )
  in
    Group (List.map changeIf list) id


deleteAt : ID -> Group a -> Group a
deleteAt a (Group list id) =
  Group (List.filter ((/=) a << fst) list) id


indexedMap : (ID -> a -> a) -> Group a -> Group a
indexedMap f g =
  g
