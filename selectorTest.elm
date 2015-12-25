import Selector exposing (..)
import ElmTest exposing (..)

-- Test

type alias Item = { name: String, selected: Bool }

noneSelected = [ { name = "Martin", selected = False }
               , { name = "Joe", selected = False} ]

firstSelected = [ { name = "Martin", selected = True }
                , { name = "Joe", selected = False} ]

lastSelected = [ { name = "Martin", selected = True }
               , { name = "Joe", selected = False} ]

selected: String -> List Item -> Bool
selected name =
  List.member { name = name, selected = True }

onlySelected: String -> List Item -> Bool 
onlySelected name list =
  (List.filter .selected list |> List.length) == 
    (List.filter (\r -> r == {name = name, selected = True}) list |> List.length)

tests = 
  suite "update"
    [ suite "Next"
        [ test "if no element selected it selects the first" 
            <| assert ( noneSelected |>  update Next |> onlySelected "Martin" )
        , test "it selects only the next element" 
            <| assert ( firstSelected |> update Next |> onlySelected "Joe" ) 
        , test "if last is selected, it stays selected" 
            <| assert ( lastSelected |> update Next |> onlySelected "Joe")]
    , suite "Prev"
        [ test "if no element selected, it selects the last element"
            <| assert (noneSelected |> update Prev |> onlySelected "Joe")
        , test "it selects the previous element"
            <| assert (lastSelected |> update Prev |> onlySelected "Martin")
        , test "If first selected it stays selected"
            <| assert (firstSelected |> update Prev |> onlySelected "Martin")]]

main = elementRunner tests
