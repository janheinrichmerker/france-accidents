module Visualization2 exposing (..)

import Html.Styled exposing (Html, br, div, text)
import Model exposing (Accident)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))
import Utils exposing (tupleMean)


type alias Coordinates =
    ( Float, Float )


type alias CoordinatesRange =
    ( Coordinates, Coordinates )


type alias CoordinatesPoint =
    ( Coordinates, List Float )


type alias CoordinatesData =
    List CoordinatesPoint


type alias Model =
    { backgroundUrl : String
    , boundaries : CoordinatesRange
    , gridCount : ( Int, Int )
    }


type Msg
    = NoOp


label : String
label =
    "Person Characteristics Stick Figures"


init : ( Model, Cmd Msg )
init =
    ( { backgroundUrl = "/france.svg"
      , boundaries = ( ( 51.5, -5.8 ), ( 41, 10 ) )
      , gridCount = ( 10, 10 )
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> List Accident -> Html Msg
view model accidents =
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        ]
