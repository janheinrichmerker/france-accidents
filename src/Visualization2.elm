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


type Group
    = GroupNone
      -- With the number of columns and rows to group into.
    | GroupCoordinates Int Int
    | GroupDepartments
    | GroupCommunes


type Display
    = DisplayAverage
    | DisplayXray


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
    , group : Group
    , display : Display
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
      , group = GroupCoordinates 10 10
      , display = DisplayAverage
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


toCoordinates : Accident -> Maybe Coordinates
toCoordinates accident =
    Maybe.map2 Tuple.pair accident.latitude accident.longitude


isInBounds : CoordinatesRange -> Coordinates -> Bool
isInBounds ( ( minLat, minLong ), ( maxLat, maxLong ) ) ( lat, long ) =
    minLat <= lat && lat <= maxLat && minLong <= long && long <= maxLong


filterByBounds : CoordinatesRange -> List Accident -> List Accident
filterByBounds range accidents =
    let
        isAccidentInBounds : Accident -> Bool
        isAccidentInBounds accident =
            accident
                |> toCoordinates
                |> Maybe.map (isInBounds range)
                |> Maybe.withDefault False
    in
    accidents |> List.filter isAccidentInBounds


accidentsWithCoordinates : List Accident -> List ( Accident, Coordinates )
accidentsWithCoordinates accidents =
    accidents
        |> List.filterMap accidentWithCoordinates


accidentWithCoordinates : Accident -> Maybe ( Accident, Coordinates )
accidentWithCoordinates accident =
    Maybe.map2 Tuple.pair accident.latitude accident.longitude
        |> Maybe.map (Tuple.pair accident)


groupedCoordinatePoints : Group -> List Accident -> List CoordinatesPoint
groupedCoordinatePoints group accidents =
    let
        withCoordinates =
            accidents |> accidentsWithCoordinates
    in
    withCoordinates
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


scatterplot : String -> CoordinatesRange -> Display -> CoordinatesData -> Html Msg
scatterplot backgroundUrl range display data =
    let
        width : Float
        width =
            500

        height : Float
        height =
            500

        padding : Float
        padding =
            60

        ( ( minLatitude, minLongitude ), ( maxLatitude, maxLongitude ) ) =
            range

        latitudeRange =
            ( minLatitude, maxLatitude )

        longitudeRange =
            ( minLongitude, maxLongitude )

        xScale : ContinuousScale Float
        xScale =
            longitudeRange |> Scale.linear ( 0, width - 2 * padding )

        yScale : ContinuousScale Float
        yScale =
            latitudeRange |> Scale.linear ( 0, height - 2 * padding )

        ticks : Int
        ticks =
            12

        xAxis : Svg msg
        xAxis =
            xScale |> Axis.bottom [ Axis.tickCount ticks ]

        yAxis : Svg msg
        yAxis =
            yScale |> Axis.left [ Axis.tickCount ticks ]

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = longitudeRange |> tupleMean
            , y = latitudeRange |> Tuple.second
            }
    in
    svg
        [ TypedSvg.Attributes.viewBox 0 0 width height
        , TypedSvg.Attributes.width (Percent 50)
        , TypedSvg.Attributes.height (Percent 50)
        ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point text { display: none; }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ TypedSvg.Attributes.transform [ Translate padding padding ] ]
            [ image
                [ TypedSvg.Attributes.href backgroundUrl
                , TypedSvg.Attributes.width (Px (width - 2 * padding))
                , TypedSvg.Attributes.height (Px (height - 2 * padding))
                , TypedSvg.Attributes.preserveAspectRatio AlignNone Meet
                ]
                []
            ]
        , g
            [ TypedSvg.Attributes.transform [ Translate padding padding ] ]
            (data |> List.map (point xScale yScale))
        , g
            [ TypedSvg.Attributes.transform [ Translate padding (height - padding) ] ]
            [ xAxis
            , text_
                [ TypedSvg.Attributes.x (Px (Scale.convert xScale labelPositions.x))
                , TypedSvg.Attributes.y (Px (padding / 2))
                , TypedSvg.Attributes.textAnchor AnchorMiddle
                ]
                [ TypedSvg.Core.text "latitude" ]
            ]
        , g
            [ TypedSvg.Attributes.transform [ Translate padding padding ] ]
            [ yAxis
            , text_
                [ TypedSvg.Attributes.x (Px 0)
                , TypedSvg.Attributes.y (Px (Scale.convert yScale labelPositions.y - (padding / 2)))
                , TypedSvg.Attributes.textAnchor AnchorMiddle
                ]
                [ TypedSvg.Core.text "longitude" ]
            ]
        ]
        |> fromUnstyled


view : Model -> List Accident -> Html Msg
view model accidents =
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        , groupedCoordinatePoints model.group accidents
            |> scatterplot model.backgroundUrl model.boundaries model.display
        ]
