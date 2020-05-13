module RandomQuoteMachine exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode as JD
import Json.Encode as JE
import Random



-- Model


type alias Quote =
    { quote : String
    , author : String
    }


type alias Model =
    { quotes : Array Quote
    , error : String
    , randomQuote : Maybe Quote
    }



{--
arr = Array.fromList model.quotes
Array.length arr

type alias Model =
  Array {author : String, quote : String}

Array.get : Int -> Array Model -> Maybe Model
Array.get 1 arr
--}


init : () -> ( Model, Cmd Msg )
init _ =
    ( { quotes =
            Array.fromList
                [ { quote = ""
                  , author = ""
                  }
                ]
      , error = ""
      , randomQuote = Nothing
      }
    , getQuote
    )


getQuote : Cmd Msg
getQuote =
    Http.get
        --{ url = "https://api.kanye.rest"
        { url = "https://gist.githubusercontent.com/camperbot/5a022b72e96c4c9585c32bf6a75f62d9/raw/e3c6895ce42069f0ee7e991229064f167fe8ccdc/quotes.json"
        , expect = Http.expectJson GotQuote quoteDecoder
        }


quoteDecoder : JD.Decoder (Array Quote)
quoteDecoder =
    JD.field "quotes" (JD.array quoteHelp)


quoteHelp =
    JD.map2 Quote
        (JD.field "quote" JD.string)
        (JD.field "author" JD.string)



-- Update


type Msg
    = GotQuote (Result Http.Error (Array Quote))
    | GenerateRandomQuote
    | RandomQuote Int


update msg model =
    case msg of
        GenerateRandomQuote ->
            ( model
            , Random.generate RandomQuote <|
                Random.int 1 (arrayLength model.quotes)
            )

        RandomQuote quote ->
            ( { model |
                randomQuote = Array.get quote model.quotes
              }
            , Cmd.none
            )

        GotQuote (Ok value) ->
            ( { model
                | quotes = value
              }
            , Random.generate RandomQuote <|
                Random.int 1 (arrayLength model.quotes)
            )

        GotQuote (Err errorMessage) ->
            ( { model
                | error = buildErrorMessage errorMessage
              }
            , Cmd.none
            )


buildErrorMessage errorMessage =
    case errorMessage of
        Http.BadUrl url ->
            "invalid url" ++ url

        Http.Timeout ->
            "took long time to respond"

        Http.NetworkError ->
            "can't connect to the internet"

        Http.BadBody body ->
            body

        Http.BadStatus code ->
            String.fromInt code

arrayLength array =
    Array.length <| array



-- Subscriptions


subscriptions _ =
    Sub.none



-- View


view model =
    Html.div
        []
        [ Html.h1 []
            [ Html.text "TODO: randomized init, indexdb local storage and state"
            ]
        --, Html.ul [] <|
            --List.map viewQuote <|
                --Array.toList model.quotes
        , Html.text <|
            case model.randomQuote of
                Just quote ->
                    "Quote: "
                        ++ quote.quote
                        ++ " Author: "
                        ++ quote.author
                Nothing ->
                    ""
        , Html.button
            [ Events.onClick GenerateRandomQuote
            ]
            [ Html.text "new quote" ]
        ]


viewQuote quote =
    Html.li []
        [ Html.text <|
            "Quote: "
                ++ quote.quote
                ++ " ,Author: "
                ++ quote.author
        ]



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
