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
    Test.describe "splits a list into groups of N and returns 2-dimensional list"
        [ Test.test "test 1" <|
            \_ ->
                BasicAlgo.chunkArrayInGroups [ "a", "b", "c", "d" ] 2
                    |> Expect.equal [ [ "a", "b" ], [ "c", "d" ] ]
        , Test.test "test 2" <|
            \_ ->
                BasicAlgo.chunkArrayInGroups [ 0, 1, 2, 3, 4, 5 ] 3
                    |> Expect.equal [ [ 0, 1, 2 ], [ 3, 4, 5 ] ]
        , Test.test "test 3" <|
            \_ ->
                BasicAlgo.chunkArrayInGroups [ 0, 1, 2, 3, 4, 5 ] 2
                    |> Expect.equal [ [ 0, 1 ], [ 2, 3 ], [ 4, 5 ] ]
        , Test.test "test 4" <|
            \_ ->
                BasicAlgo.chunkArrayInGroups [ 0, 1, 2, 3, 4, 5 ] 4
                    |> Expect.equal [ [ 0, 1, 2, 3 ], [ 4, 5 ] ]
        , Test.test "test 5" <|
            \_ ->
                BasicAlgo.chunkArrayInGroups [ 0, 1, 2, 3, 4, 5, 6 ] 3
                    |> Expect.equal [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6 ] ]
        , Test.test "test 6" <|
            \_ ->
                BasicAlgo.chunkArrayInGroups [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] 4
                    |> Expect.equal [ [ 0, 1, 2, 3 ], [ 4, 5, 6, 7 ], [ 8 ] ]
        , Test.test "test 7" <|
            \_ ->
                BasicAlgo.chunkArrayInGroups [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ] 2
                    |> Expect.equal [ [ 0, 1 ], [ 2, 3 ], [ 4, 5 ], [ 6, 7 ], [ 8 ] ]
        ]


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
