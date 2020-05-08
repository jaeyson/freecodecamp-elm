module BasicAlgo exposing (..)

import Html exposing (Html)

--factorialize num =


reverseString : String -> String
reverseString =
    String.reverse


convertToF : Float -> Float
convertToF celsius =
    celsius * (9 / 5) + 32

main =
  Html.text "BasicAlgo.elm"