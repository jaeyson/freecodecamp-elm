module BasicAlgo exposing (..)


chunkArrayInGroups : List a -> Int -> List (List a)
chunkArrayInGroups list size =
    case List.take size list of
        [] ->
            []

        head ->
            head :: chunkArrayInGroups (List.drop size list) size


booWhoo any =
    Debug.todo "how to compare a to Bool?"


confirmEnding : String -> String -> Bool
confirmEnding string targetString =
    String.endsWith targetString string


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
