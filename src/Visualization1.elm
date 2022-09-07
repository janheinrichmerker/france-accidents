module Visualization1 exposing (..)

import Axis
import Color exposing (black)
import Html.Styled exposing (Html, br, div, fromUnstyled, h3, text)
import Model exposing (Accident)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Statistics exposing (extent, quantile)
import Task exposing (perform)
import Time exposing (Posix, now, posixToMillis)
import TypedSvg exposing (g, line, svg, text_)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Align(..), AnchorAlignment(..), Length(..), MeetOrSlice(..), Paint(..), Scale(..), Transform(..))


type alias Model =
    { timestamp : Maybe Posix }


type Msg
    = NoOp
    | GotTime Posix


label : String
label =
    "Severity Time Series"


init : ( Model, Cmd Msg )
init =
    ( { timestamp = Nothing }
    , getTime
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTime posix ->
            ( { model | timestamp = Just posix }, Cmd.none )


toPoint2D : Accident -> Maybe Point2D
toPoint2D accident =
    let
        x =
            toFloat (posixToMillis accident.timestamp)

        y =
            accident.road_traffic_width_meters
    in
    Just (Point2D "" x y)


getTime : Cmd Msg
getTime =
    perform GotTime now


filterByTimestamp : Model -> List Accident -> List Accident
filterByTimestamp model accidents =
    case model.timestamp of
        Just posix ->
            List.filter (\accident -> posixToMillis accident.timestamp <= posixToMillis posix) accidents

        Nothing ->
            accidents


sortByTimestamp : List Accident -> List Accident
sortByTimestamp accidents =
    List.sortBy (\accident -> posixToMillis accident.timestamp) accidents


toPoints2D : Model -> List Accident -> List Point2D
toPoints2D model accidents =
    List.filterMap
        toPoint2D
        (accidents |> filterByTimestamp model |> sortByTimestamp)


view : Model -> List Accident -> Html Msg
view model accidents =
    let
        points =
            toPoints2D model accidents

        data =
            AxisData2D "Time" "Road Width" points

        aspectRatio : Maybe Float
        aspectRatio =
            computeAspectRatio points
    in
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        , h3 []
            [ text
                (Maybe.withDefault
                    "Aspect-Ratio nicht berechnet."
                    (Maybe.map
                        (\ar -> "Aspect Ratio: " ++ String.fromFloat ar)
                        aspectRatio
                    )
                )
            ]
        , fromUnstyled
            (linePlot
                w
                --(Maybe.withDefault 1 aspectRatio)
                4
                --2
                data
            )
        ]


mapConsecutive : (a -> a -> b) -> List a -> Maybe (List b)
mapConsecutive mapper list =
    Maybe.map (\l2 -> List.map2 mapper list l2) <| List.tail list


computeAspectRatio : List Point2D -> Maybe Float
computeAspectRatio data =
    let
        slopes : Maybe (List Float)
        slopes =
            mapConsecutive (\a b -> abs ((b.y - a.y) / (b.x - a.x))) data

        -- Calculate the Median.
        medianSlope : Maybe Float
        medianSlope =
            Maybe.andThen
                (\justSlopes -> quantile 0.5 (List.sort justSlopes))
                slopes

        -- Calculate the range of x values, i.e., min and max.
        rangeX : Maybe ( Float, Float )
        rangeX =
            extent (List.map (\point -> point.x) data)

        -- Calculate the range of y values, i.e., min and max.
        rangeY : Maybe ( Float, Float )
        rangeY =
            Statistics.extent (List.map (\point -> point.y) data)
    in
    Maybe.map3
        (\sm ( xmin, xmax ) ( ymin, ymax ) ->
            -- Calculate the aspect ratio
            sm * (xmax - xmin) / (ymax - ymin)
        )
        medianSlope
        rangeX
        rangeY


w : Float
w =
    900


padding : Float
padding =
    60


linePlot : Float -> Float -> AxisData2D -> Svg msg
linePlot width aspectRatio model =
    let
        height =
            width / aspectRatio

        xTicks =
            10

        xScale : ContinuousScale Float
        xScale =
            model.data
                |> List.map .x
                |> Statistics.extent
                |> Maybe.withDefault ( 0, 0 )
                |> Scale.linear ( 0, width )
                |> Scale.nice xTicks

        yTicks =
            max 2 (round (10 / aspectRatio))

        yScale : ContinuousScale Float
        yScale =
            model.data
                |> List.map .y
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\b -> ( 0, b ))
                |> Scale.linear ( height, 0 )
                |> Scale.nice yTicks

        lineGenerator : ( Float, Float ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale x, Scale.convert yScale y )

        line : Path
        line =
            List.map (\p -> ( p.x, p.y )) model.data
                |> List.map lineGenerator
                |> Shape.line Shape.monotoneInXCurve
    in
    svg
        [ TypedSvg.Attributes.viewBox 0 0 (width + 2 * padding) (height + 2 * padding)
        , TypedSvg.Attributes.width (Percent 50)
        , TypedSvg.Attributes.height (Percent 50)
        , TypedSvg.Attributes.preserveAspectRatio (Align ScaleMin ScaleMin) Slice
        ]
        [ g [ TypedSvg.Attributes.transform [ Translate (padding - 1) (height + padding) ] ]
            [ Axis.bottom [ Axis.tickCount xTicks ] xScale ]
        , g [ TypedSvg.Attributes.transform [ Translate (padding - 1) padding ] ]
            [ Axis.left [ Axis.tickCount yTicks ] yScale
            , text_
                [ TypedSvg.Attributes.fontFamily [ "sans-serif" ]
                , TypedSvg.Attributes.fontSize (Px 10)
                , TypedSvg.Attributes.x (Px 5)
                , TypedSvg.Attributes.y (Px 5)
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g
            [ TypedSvg.Attributes.transform [ Translate padding padding ]
            , TypedSvg.Attributes.class [ "series" ]
            ]
            [ Path.element line
                [ TypedSvg.Attributes.stroke (Paint black)
                , TypedSvg.Attributes.strokeWidth (Px 1)
                , TypedSvg.Attributes.fill PaintNone
                ]
            ]
        ]


type alias Point3D =
    { pointName : String, x : Float, y : Float, z : Float }


type alias Point2D =
    { pointName : String, x : Float, y : Float }


type alias AxisData2D =
    { xDescription : String
    , yDescription : String
    , data : List Point2D
    }


type alias AxisData3D =
    { xDescription : String
    , yDescription : String
    , zDescription : String
    , data : List Point3D
    }
