module BasicAlgo exposing (..)


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
