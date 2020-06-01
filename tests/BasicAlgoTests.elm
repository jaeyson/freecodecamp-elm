module BasicAlgoTests exposing (..)

import BasicAlgo
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


countHowManyTestFunctions =
    Test.describe "count the functions inside this module excluding this function"
        [ Test.test "no desc" <|
            \_ -> Expect.equal 4 (Debug.log "how many functions?" 4)
        ]


chunkArrayInGroups : Test
chunkArrayInGroups =
    Test.fuzz3
        (Fuzz.list Fuzz.string)
        (Fuzz.list Fuzz.int)
        (Fuzz.intRange 0 10000)
        "splits list into groups of N"
    <|
        \listString listInt size ->
            let
                lengthStr =
                    List.length listString

                lengthInt =
                    List.length listInt

                userInputStr =
                    BasicAlgo.chunkArrayInGroups listString size
                        |> List.length

                userInputInt =
                    BasicAlgo.chunkArrayInGroups listInt size
                        |> List.length

                float =
                    size |> toFloat
            in
            case ( lengthStr, lengthInt, size ) of
                ( 0, _, 1 ) ->
                    Expect.all
                        [ Expect.atLeast userInputStr, Expect.atLeast userInputInt ]
                        (List.length listInt)

                ( _, 0, 1 ) ->
                    Expect.all
                        [ Expect.atLeast userInputStr, Expect.atLeast userInputInt ]
                        (List.length listString)

                ( 0, 0, _ ) ->
                    Expect.all
                        [ Expect.equal userInputStr, Expect.equal userInputInt ]
                        0

                ( 1, 0, _ ) ->
                    Expect.all
                        [ Expect.atLeast userInputStr, Expect.atLeast userInputInt ]
                        1

                ( 0, 1, _ ) ->
                    Expect.all
                        [ Expect.atLeast userInputStr, Expect.atLeast userInputInt ]
                        1

                ( 1, 1, _ ) ->
                    Expect.all
                        [ Expect.equal userInputStr, Expect.equal userInputInt ]
                        1

                ( _, _, 0 ) ->
                    Expect.all
                        [ Expect.equal userInputStr, Expect.equal userInputInt ]
                        1

                ( _, _, 1 ) ->
                    Expect.all
                        [ Expect.equal userInputStr, Expect.equal userInputInt ]
                        (List.length listString)

                ( _, 0, _ ) ->
                    Expect.all
                        [ Expect.atLeast userInputStr, Expect.atLeast userInputInt ]
                        ((List.length >> toFloat) listString / float |> ceiling)

                ( 0, _, _ ) ->
                    Expect.all
                        [ Expect.atLeast userInputStr, Expect.atLeast userInputInt ]
                        ((List.length >> toFloat) listInt / float |> ceiling)

                ( _, _, _ ) ->
                    Expect.all
                        [ Expect.atLeast userInputStr, Expect.atLeast userInputInt ]
                        -- TODO: only listInt, include also listString!!!
                        ((List.length >> toFloat) listInt / float |> ceiling)


confirmEnding : Test
confirmEnding =
    Test.fuzz2 Fuzz.string Fuzz.string "Check if a string ends with the given target string." <|
        \targetString string ->
            let
                fuzzOutput =
                    string
                        |> String.right (String.length targetString)
                        |> (==) targetString
            in
            BasicAlgo.confirmEnding string targetString
                |> Expect.equal fuzzOutput


factorialize : Test
factorialize =
    Test.describe "return factorial of the provided integer"
        [ Test.test "0! = 1" <|
            \_ ->
                BasicAlgo.factorialize 0
                    |> Expect.equal 1
        , Test.test "5! = 120" <|
            \_ ->
                BasicAlgo.factorialize 5
                    |> Expect.equal 120
        , Test.test "10! = 3628800" <|
            \_ ->
                BasicAlgo.factorialize 10
                    |> Expect.equal 3628800
        , Test.test "20! = 2432902008176640000" <|
            \_ ->
                BasicAlgo.factorialize 20
                    |> Expect.equal 2432902008176640000
        ]


convertToF : Test
convertToF =
    Test.describe "convert celsius to fahrenheit"
        [ Test.test "0 = 32" <|
            \_ ->
                BasicAlgo.convertToF 0
                    |> Expect.equal 32
        , Test.test "20 = 68" <|
            \_ ->
                BasicAlgo.convertToF 20
                    |> Expect.equal 68
        ]


reverseString : Test
reverseString =
    Test.fuzz Fuzz.string "reverses a string twice" <|
        \string ->
            (String.reverse >> String.reverse) string
                |> Expect.equal string



{--
convertToF : Test
convertToF =
  Test.fuzz Fuzz.int "convert celsius to fahrenheit" <|
    \celsius ->
      BasicAlgo.convertToF celsius
        |>
--}
