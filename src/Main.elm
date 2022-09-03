module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key)
import Data exposing (HttpJsonError, errorToString, expectAccidentJsonLines)
import Html exposing (Html, a, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Model exposing (Accident, Resource(..))
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


viewAccidents : Model -> Html msg
viewAccidents model =
    case model.accidents of
        Failure message ->
            text ("Unable to load accidents: " ++ message)

        Loading ->
            text "Loading accidents..."

        Success fullText ->
            div []
                [ case model.currentVisualization of
                    CurrentVisualization1 ->
                        Visualization1.view model.visualization1 fullText

                    CurrentVisualization2 ->
                        Visualization2.view model.visualization2 fullText

                    CurrentVisualization3 ->
                        Visualization3.view model.visualization3 fullText
                ]


view : Model -> Document Msg
view model =
    { title = "🇫🇷 Accidents in France"
    , body =
        [ h1 [] [ text "Accidents" ]
        , ul
            []
            [ li
                []
                [ a
                    [ onClick (SelectVisualization CurrentVisualization1), href "#" ]
                    [ text "Visualization 1" ]
                ]
            , li
                []
                [ a
                    [ onClick (SelectVisualization CurrentVisualization2), href "#" ]
                    [ text "Visualization 2" ]
                ]
            , li
                []
                [ a
                    [ onClick (SelectVisualization CurrentVisualization3), href "#" ]
                    [ text "Visualization 3" ]
                ]
            ]
        , viewAccidents model
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
