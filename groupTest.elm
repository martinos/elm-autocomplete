module Main (..) where

import Group exposing (..)
import ElmTest exposing (..)


tests =
  suite
    "main"
    [ suite
        "add"
        [ test "it adds one record on empty list"
            <| assertEqual (emptyGroup |> add "one") (Group [ ( 0, "one" ) ] 1)
        , test "it adds one record on top an  existing one"
            <| assertEqual (emptyGroup |> add "one" |> add "two") (Group [ ( 1, "two" ), ( 0, "one" ) ] 2)
        ]
    , suite
        "deleteAt"
        [ test "it returns an empty group when deleting a group with one entry"
            <| assertEqual (emptyGroup |> add "one" |> deleteAt (0)) (Group [] 1)
        , test "it returns the same group when the deleted id does not exist"
            <| assertEqual (emptyGroup |> add "one" |> deleteAt (1)) (Group [ ( 0, "one" ) ] 1)
        ]
    , suite
        "changeIf"
        [ test "changes a record for a given id"
            <| assertEqual
                (Group [ ( 0, "two" ) ] 1)
                (emptyGroup
                  |> add "one"
                  |> changeAt (always "two") 0
                )
        ]
    , suite
        "indexedMap"
        []
    , suite
        "map"
        [ test "should apply a function to the list"
            <| assertEqual
                (Group [ ( 0, 12 ) ] 1)
                (emptyGroup
                  |> add "one"
                  |> map (\list -> List.map (\( id, str ) -> ( id, 12 )) list)
                )
        ]
    , suite
        "toListWithID"
        [ test "should return a list with ids"
            <| assertEqual ([ ( 0, "one" ) ]) (emptyGroup |> add "one" |> listWithId)
        ]
    ]

main =
  elementRunner tests

