module Selector (..) where

import ElmTest exposing (..)
import Group exposing (..)
import Html exposing (..)
import Debug exposing (..)


type Selector a
  = Selector (Maybe ID) (Group a)


emptySelector : Selector a
emptySelector =
  Selector Nothing emptyGroup


cursor (Selector id _) =
  id


group (Selector _ group) =  group


move : (List ( ID, a ) -> List ( ID, a )) -> Selector a -> Selector a
move orientation (Selector pos group) =
  let
    (Group list id) = group

    listIds = list |> orientation |> List.map fst
  in
    case pos of
      Nothing ->
        Selector (listIds |> List.head) group

      Just id ->
        Selector (listIds |> findNext pos) group


prev : Selector a -> Selector a
prev =
  move List.reverse


next : Selector a -> Selector a
next =
  move identity


findNext : Maybe a -> List a -> Maybe a
findNext cursPos list =
  let
    g curPos ( prev, result ) =
      if prev == cursPos then
        ( Just curPos, Just curPos )
      else
        ( Just curPos, result )

    fromFold = List.foldl g ( Nothing, Nothing ) list
  in
    if snd fromFold == Nothing then
      fst fromFold
    else
      snd fromFold


map : (Group a -> Group b) -> Selector a -> Selector b
map f (Selector id group) =
  Selector id (f group)


selection : Selector a -> Maybe a
selection (Selector maybeId (Group list nextID)) =
  maybeId
    `Maybe.andThen` (\id ->
                      List.filter ((==) id << fst) list
                        |> List.head
                        |> flip Maybe.andThen (Just << snd)
                    )

