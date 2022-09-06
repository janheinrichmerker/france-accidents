module Visualization1 exposing (..)

import Html.Styled exposing (Html, br, div, text)
import Model exposing (Accident)


type alias Model =
    {}


type Msg
    = NoOp


label : String
label =
    "Severity Time Series"


init : Model
init =
    {}


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> List Accident -> Html msg
view model accidents =
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        ]
