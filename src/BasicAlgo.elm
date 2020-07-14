module BasicAlgo exposing (..)

{--
frankenSplice([1, 2, 3], [4, 5], 1) // [ 4, 1, 2, 3, 5 ]
frankenSplice([1,2],[4,5,6,7],2) // [ 4, 5, 1, 2, 6, 7 ]
frankenSplice([1,2],[4,5,6,7],3) // [ 4, 5, 6, 1, 2, 7 ]
frankenSplice([1,2],[4,5,6,7],4) // [ 4, 5, 6, 7, 1, 2 ]
frankenSplice([1,2],[4,5,6,7],5) // [ 4, 5, 6, 7, 1, 2 ]
frankenSplice([1,2],[4,5,6,7],0) // [ 1, 2, 4, 5, 6, 7 ]
frankenSplice([1,2],[4,5,6,7],1) // [ 4, 1, 2, 5, 6, 7 ]
frankenSplice([1,2,3],[4,5,6,7],1) // [ 4, 1, 2, 3, 5, 6, 7 ]
frankenSplice(["me",2,3],[4,5,6,7],1) //[ 4, 'me', 2, 3, 5, 6,7 ]

frankenSplice : List a -> List a-> Int -> List a
frankenSplice listA listB num =
--}


chunkArrayInGroups : List a -> Int -> List (List a)
chunkArrayInGroups list size =
    case ( List.take size list, size ) of
        ( _, 0 ) ->
            list :: []

        ( [], _ ) ->
            []

        ( head, _ ) ->
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
