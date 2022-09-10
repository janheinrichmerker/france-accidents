module Visualization2 exposing (..)

import Axis
import Color exposing (black)
import Html.Styled exposing (Html, br, div, fromUnstyled, text)
import Model exposing (Accident)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, g, image, svg, text_)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Align(..), AnchorAlignment(..), Length(..), MeetOrSlice(..), Opacity(..), Paint(..), Transform(..))
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


accidentsWithCoordinates : List Accident -> List ( Accident, Coordinates )
accidentsWithCoordinates accidents =
    accidents
        |> List.filterMap accidentWithCoordinates


accidentWithCoordinates : Accident -> Maybe ( Accident, Coordinates )
accidentWithCoordinates accident =
    Maybe.map2 Tuple.pair accident.latitude accident.longitude
        |> Maybe.map (Tuple.pair accident)


accidentsToCoordinatePoints : List Accident -> List CoordinatesPoint
accidentsToCoordinatePoints accidents =
    accidents
        |> accidentsWithCoordinates
        |> List.map (\( _, c ) -> ( c, [] ))


marker : Svg msg
marker =
    circle
        [ TypedSvg.Attributes.r (Px 2)
        , TypedSvg.Attributes.fill (Paint black)
        , TypedSvg.Attributes.fillOpacity (Opacity 0.25)
        , TypedSvg.Attributes.stroke (Paint black)
        ]
        []


point : ContinuousScale Float -> ContinuousScale Float -> CoordinatesPoint -> Svg msg
point scaleX scaleY ( coordinates, dimensions ) =
    let
        ( latitude, longitude ) =
            coordinates
    in
    g
        [ TypedSvg.Attributes.class [ "point" ]
        , TypedSvg.Attributes.fontSize <| Px 10.0
        , TypedSvg.Attributes.fontFamily [ "sans-serif" ]
        , TypedSvg.Attributes.transform
            [ Translate
                (Scale.convert scaleX longitude)
                (Scale.convert scaleY latitude)
            ]
        ]
        [ marker
        , text_
            [ TypedSvg.Attributes.x (Px 0)
            , TypedSvg.Attributes.y (Px 0)
            , TypedSvg.Attributes.textAnchor AnchorMiddle
            ]
            [ -- todo
              TypedSvg.Core.text "Point"
            ]
        ]


view : Model -> List Accident -> Html Msg
view model accidents =
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        ]
