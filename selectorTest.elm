import Selector exposing (..)
import ElmTest exposing (..)

-- Test

noneSelected = [ { name = "Martin", selected = False }
               , { name = "Joe", selected = False} ]

firstSelected = [ { name = "Martin", selected = True }
                , { name = "Joe", selected = False} ]

lastSelected = [ { name = "Martin", selected = True }
               , { name = "Joe", selected = False} ]

selected name =
  List.member { name = name, selected = True }

onlySelected name list =
  (List.filter .selected list |> List.length) == 
    (List.filter (\r -> r == {name = name, selected = True}) list |> List.length)

tests = 
  suite "update"
    [ suite "Next"
        [ test "In no element selected it selects the first" 
            <| assert ( noneSelected |>  update Next |> onlySelected "Martin" )
        , test "it selects only the next element" 
            <| assert ( firstSelected |> update Next |> onlySelected "Joe" ) 
        , test "If last is selected it should stay selected" 
            <| assert ( lastSelected |> update Next |> onlySelected "Joe")]
    , suite "Prev"
        [ test "If no element selected it selects the last element"
            <| assert (noneSelected |> update Prev |> onlySelected "Joe")
        , test "it select the previous element"
            <| assert (lastSelected |> update Prev |> onlySelected "Martin")
        , test "If first selected it stays selected"
            <| assert (firstSelected |> update Prev |> onlySelected "Martin")]]

main = elementRunner tests
