module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key)
import Data exposing (HttpJsonError, errorToString, expectAccidentJsonLines)
import Html exposing (Html, br, h1, li, pre, strong, text, ul)
import Http
import List
import Model exposing (Accident)
import String
import Url exposing (Url)


type Resource t
    = Failure String
    | Loading
    | Success t


type alias Model =
    { accidents : Resource (List Accident)
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { accidents = Loading }
    , Http.get
        { url = "/data/accidents-sample.jsonl"
        , expect = expectAccidentJsonLines GotAccidentsData
        }
    )


type Msg
    = NoOp
    | GotAccidentsData (Result HttpJsonError (List Accident))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotAccidentsData result ->
            let
                resource =
                    case result of
                        Ok data ->
                            Success data

                        Err err ->
                            Failure (errorToString err)
            in
            ( { model | accidents = resource }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp


viewCharacteristics : Resource (List Accident) -> Html msg
viewCharacteristics accidents =
    let
        dataText =
            case accidents of
                Failure message ->
                    text ("Unable to load accidents: " ++ message)

                Loading ->
                    text "Loading..."

                Success fullText ->
                    pre []
                        [ text (String.fromInt (List.length fullText))
                        ]
    in
    li
        []
        [ strong [] [ text "Accidents: " ]
        , br [] []
        , dataText
        ]


view : Model -> Document Msg
view model =
    { title = "🇫🇷 Accidents in France"
    , body =
        [ h1 [] [ text "Stocks" ]
        , ul [] [ viewCharacteristics model.accidents ]
        ]
    }


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
