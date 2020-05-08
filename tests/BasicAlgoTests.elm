module BasicAlgoTests exposing (..)

import BasicAlgo
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


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
