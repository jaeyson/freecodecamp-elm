module RandomQuoteMachine exposing (main)

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
    , number : Maybe Int
    }


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
      , number = Nothing
      }
    , getQuote
    )

getQuote : Cmd Msg
getQuote =
    Http.get
        --{ url = "https://gist.githubusercontent.com/camperbot/5a022b72e96c4c9585c32bf6a75f62d9/raw/e3c6895ce42069f0ee7e991229064f167fe8ccdc/quotes.json"
        { url = "http://localhost:3000/quotes"
        , expect = Http.expectJson GotQuote quoteDecoder
        }


quoteDecoder : JD.Decoder (Array Quote)
quoteDecoder =
    --JD.field "quotes" (JD.array quoteHelp)
    JD.array quoteHelp


quoteHelp =
    JD.map2 Quote
        (JD.field "quote" JD.string)
        (JD.field "author" JD.string)


getRandomQuote : Array Quote -> Cmd Msg
getRandomQuote quotes =
    Random.generate RandomQuote <|
        Random.int 0 (arrayLength quotes)



-- Update


type Msg
    = GotQuote (Result Http.Error (Array Quote))
    | GenerateRandomQuote
    | RandomQuote Int


update msg model =
    case msg of
        GenerateRandomQuote ->
            ( model
            , getRandomQuote model.quotes
            )

        RandomQuote randomIndexNumber ->
            ( { model
                | randomQuote = Array.get randomIndexNumber model.quotes
                , number = Just randomIndexNumber
              }
            , Cmd.none
            )

        GotQuote (Ok quotes) ->
            ( { model
                | quotes = quotes
                , number = Nothing
              }
            , getRandomQuote model.quotes
            )

        GotQuote (Err errorMessage) ->
            ( { model
                | error = buildErrorMessage errorMessage
                , number = Nothing
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
    Array.length array



-- View
view model =
    Html.div
        []
        [ Html.h1 []
            [ Html.text "TODO: randomized init, indexdb local storage and state"
            ]
        , Html.text model.error
        , Html.text <|
            case model.randomQuote of
                Just quote ->
                    "Quote: "
                        ++ quote.quote
                        ++ " Author: "
                        ++ quote.author

                Nothing ->
                    ""
        , Html.h3 []
            [ Html.text <|
                case model.number of
                    Just number ->
                        String.fromInt number
                    Nothing ->
                        ""
            ]
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
        , subscriptions = \_ -> Sub.none
        }
