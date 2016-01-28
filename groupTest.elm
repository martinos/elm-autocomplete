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
        ]
    , suite
        "changeAt"
        [ test "changes a record for a given id"
            <| assertEqual
                (Group [ ( 0, "two" ) ] 1)
                (emptyGroup
                  |> add "one"
                  |> changeAt 0 (always "two")
                )
        ]
    , suite
        "indexedMap"
        []
    ]

main =
  elementRunner tests

