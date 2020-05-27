module BasicAlgo exposing (..)


confirmEnding : String -> String -> Bool
confirmEnding string targetString =
    String.endsWith targetString string



{--
string
  |> String.right (String.length targetString)
  |> (==) targetString
--}


factorialize : Int -> Int
factorialize num =
    case num of
        0 ->
            1

        _ ->
            num * (factorialize <| num - 1)


reverseString : String -> String
reverseString =
    String.reverse


convertToF : Float -> Float
convertToF celsius =
    celsius * (9 / 5) + 32
