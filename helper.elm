module Helper where 

import Array exposing (..)
import Html exposing (..)
import ElmTest exposing (..)
import Graphics.Element exposing (..)


change: Int -> (a -> a) -> Array a -> Array a
change index f array =
  case get index array of
    Just elem -> 
      set index (f elem) array
    Nothing -> array

tests = 
  let orig = fromList [1,2] 
  in 
    suite "change" 
      [ test "change a non existing an element does not change anything" 
          (assertEqual (orig |> change 10 (always 100)) (fromList [1,2]))
      , test "change an existing value"
          (assertEqual (orig |> change 0 (always 100)) (fromList [100,2])) ]

main = elementRunner tests

-- main = text <| toString (Array.fromList [1 , 3, 4] |> change 12 (always 3))
