module Main exposing (main)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation exposing (Key)
import Css exposing (FontWeight, bold, borderBottom3, displayFlex, em, ex, fontWeight, listStyle, margin2, none, normal, padding, padding2, px, rgb, solid, zero)
import Data exposing (HttpJsonError, errorToString, expectAccidentJsonLines)
import Html.Styled exposing (Html, button, div, h1, h2, header, li, main_, nav, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
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
    let
        ( model1, cmd1 ) =
            Visualization1.init

        ( model2, cmd2 ) =
            Visualization2.init

        ( model3, cmd3 ) =
            Visualization3.init

        loadAccidentsCmd =
            Http.get
                { url = "/data/accidents-sample.jsonl"
                , expect = expectAccidentJsonLines GotAccidentsData
                }
    in
    ( { accidents = Loading
      , currentVisualization = CurrentVisualization2
      , visualization1 = model1
      , visualization2 = model2
      , visualization3 = model3
      }
    , Cmd.batch
        [ loadAccidentsCmd
        , Cmd.map VisualizationMsg1 cmd1
        , Cmd.map VisualizationMsg2 cmd2
        , Cmd.map VisualizationMsg3 cmd3
        ]
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
            let
                ( model1, cmd1 ) =
                    Visualization1.update msg1 model.visualization1
            in
            ( { model | visualization1 = model1 }, Cmd.map VisualizationMsg1 cmd1 )

        VisualizationMsg2 msg2 ->
            let
                ( model2, cmd2 ) =
                    Visualization2.update msg2 model.visualization2
            in
            ( { model | visualization2 = model2 }, Cmd.map VisualizationMsg2 cmd2 )

        VisualizationMsg3 msg3 ->
            let
                ( model3, cmd3 ) =
                    Visualization3.update msg3 model.visualization3
            in
            ( { model | visualization3 = model3 }, Cmd.map VisualizationMsg3 cmd3 )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp


visualization : Model -> Html Msg
visualization model =
    case model.accidents of
        Failure message ->
            text ("Unable to load accidents: " ++ message)

        Loading ->
            text "Loading accidents..."

        Success accidents ->
            div []
                [ case model.currentVisualization of
                    CurrentVisualization1 ->
                        Html.Styled.map VisualizationMsg1
                            (Visualization1.view model.visualization1 accidents)

                    CurrentVisualization2 ->
                        Html.Styled.map VisualizationMsg2
                            (Visualization2.view model.visualization2 accidents)

                    CurrentVisualization3 ->
                        Html.Styled.map VisualizationMsg3
                            (Visualization3.view model.visualization3 accidents)
                ]


visualizationLabel : CurrentVisualization -> String
visualizationLabel vis =
    case vis of
        CurrentVisualization1 ->
            Visualization1.label

        CurrentVisualization2 ->
            Visualization2.label

        CurrentVisualization3 ->
            Visualization3.label


selectVisualizationButton : Model -> CurrentVisualization -> Html Msg
selectVisualizationButton model vis =
    let
        buttonFontWeight =
            if vis == model.currentVisualization then
                fontWeight bold

            else
                fontWeight normal
    in
    li
        [ css
            [ padding2 (ex 0.25) (em 0.5)
            , margin2 (ex 0.5) (em 1)
            ]
        ]
        [ button
            [ onClick (SelectVisualization vis)
            , css [ buttonFontWeight ]
            ]
            [ text (visualizationLabel vis) ]
        ]


siteHeader : Model -> Html Msg
siteHeader model =
    header
        [ css
            [ borderBottom3 (px 1) solid (rgb 0 0 0) ]
        ]
        [ h1 [] [ text "Accidents" ]
        , h2 [] [ text (visualizationLabel model.currentVisualization) ]
        ]


siteNav : Model -> Html Msg
siteNav model =
    nav
        [ css
            [ borderBottom3 (px 1) solid (rgb 0 0 0) ]
        ]
        [ ul
            [ css
                [ displayFlex
                , listStyle none
                , padding zero
                ]
            ]
            [ selectVisualizationButton model CurrentVisualization1
            , selectVisualizationButton model CurrentVisualization2
            , selectVisualizationButton model CurrentVisualization3
            ]
        ]


siteMain : Model -> Html Msg
siteMain model =
    main_
        [ css
            [ margin2 (ex 1) zero ]
        ]
        [ visualization model ]


view : Model -> Document Msg
view model =
    { title = "🇫🇷 Accidents in France"
    , body =
        [ siteHeader model |> toUnstyled
        , siteNav model |> toUnstyled
        , siteMain model |> toUnstyled
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
