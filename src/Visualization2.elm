module Visualization2 exposing (..)

import Html exposing (Html, text)
import Model exposing (Accident)


type alias Model =
    {}


type Msg
    = NoOp


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
    text (String.fromInt (List.length accidents))
