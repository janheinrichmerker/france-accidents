module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key)
import Data exposing (HttpJsonError, errorToString, expectAccidentJsonLines)
import Html exposing (Html, br, h1, li, pre, strong, text, ul)
import Http
import List
import Model exposing (Accident, Resource(..))
import String
import Url exposing (Url)
import Visualization1
import Visualization2
import Visualization3


type CurrentVisualization
    = CurrentVisualization1
    | CurrentVisualization2
    | CurrentVisualization3


type alias Model =
    { accidents : Resource (List Accident)
    , currentVisualization : CurrentVisualization
    , visualization1 : Visualization1.Model
    , visualization2 : Visualization2.Model
    , visualization3 : Visualization3.Model
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { accidents = Loading
      , currentVisualization = CurrentVisualization1
      , visualization1 = Visualization1.init
      , visualization2 = Visualization2.init
      , visualization3 = Visualization3.init
      }
    , Http.get
        { url = "/data/accidents-sample.jsonl"
        , expect = expectAccidentJsonLines GotAccidentsData
        }
    )


type Msg
    = NoOp
    | GotAccidentsData (Result HttpJsonError (List Accident))
    | SelectVisualization CurrentVisualization
    | VisualizationMsg1 Visualization1.Msg
    | VisualizationMsg2 Visualization2.Msg
    | VisualizationMsg3 Visualization3.Msg


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

        SelectVisualization vis ->
            ( { model | currentVisualization = vis }, Cmd.none )

        VisualizationMsg1 msg1 ->
            ( { model | visualization1 = Visualization1.update msg1 model.visualization1 }, Cmd.none )

        VisualizationMsg2 msg2 ->
            ( { model | visualization2 = Visualization2.update msg2 model.visualization2 }, Cmd.none )

        VisualizationMsg3 msg3 ->
            ( { model | visualization3 = Visualization3.update msg3 model.visualization3 }, Cmd.none )


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
        [ h1 [] [ text "Accidents" ]
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
