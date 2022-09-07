module Visualization1 exposing (..)

import Axis
import Color exposing (black)
import Html.Styled exposing (Html, br, div, form, fromUnstyled, h3, option, select, text)
import Html.Styled.Attributes exposing (selected)
import Html.Styled.Events exposing (onClick)
import Model exposing (Accident, Person, Severity(..), Vehicle)
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


type AspectRatio
    = AspectRatioSquare
    | AspectRatio2to1
    | AspectRatio16to9
    | AspectRatioBanking45


type DimensionReference
    = DimensionReferenceRelative
    | DimensionReferenceAbsolute


type DimensionY
    = DimensionYInjured DimensionReference
    | DimensionYKilled DimensionReference


type alias Model =
    { timestamp : Maybe Posix
    , aspectRatio : AspectRatio
    , dimensionY : DimensionY
    }


type Msg
    = NoOp
    | GotTime Posix
    | SelectAspectRatio AspectRatio
    | SelectDimensionY DimensionY


label : String
label =
    "Severity Time Series"


init : ( Model, Cmd Msg )
init =
    ( { timestamp = Nothing
      , aspectRatio = AspectRatioBanking45
      , dimensionY = DimensionYInjured DimensionReferenceAbsolute
      }
    , getTime
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotTime posix ->
            ( { model | timestamp = Just posix }, Cmd.none )

        SelectAspectRatio aspectRatio ->
            ( { model | aspectRatio = aspectRatio }, Cmd.none )

        SelectDimensionY dimensionY ->
            ( { model | dimensionY = dimensionY }, Cmd.none )


getTime : Cmd Msg
getTime =
    perform GotTime now


isInjured : Person -> Bool
isInjured person =
    case person.severity of
        SeverityInjuredHospitalized ->
            True

        SeveritySlightlyInjured ->
            True

        _ ->
            False


isKilled : Person -> Bool
isKilled person =
    case person.severity of
        SeverityKilled ->
            True

        _ ->
            False


toPoint2D : Accident -> Maybe Point2D
toPoint2D accident =
    let
        x =
            toFloat (posixToMillis accident.timestamp)

        persons : List Person
        persons =
            List.foldl (\vehicle agg -> List.append agg vehicle.persons) [] accident.vehicles

        numPersons =
            List.length persons

        numInjured =
            List.length (List.filter isInjured persons)

        numKilled =
            List.length (List.filter isKilled persons)

        y =
            toFloat numInjured / toFloat numPersons
    in
    Just (Point2D "" x y)


{-| Remove accidents that have a timestamp in the future, i.e., an invalid timestamp.
-}
filterByTimestamp : Model -> List Accident -> List Accident
filterByTimestamp model accidents =
    case model.timestamp of
        Just posix ->
            List.filter (\accident -> posixToMillis accident.timestamp <= posixToMillis posix) accidents

        Nothing ->
            accidents


{-| Sort accidents in chronological order, i.e., by ascending timestamp.
-}
sortByTimestamp : List Accident -> List Accident
sortByTimestamp accidents =
    List.sortBy (\accident -> posixToMillis accident.timestamp) accidents


toPoints2D : Model -> List Accident -> List Point2D
toPoints2D model accidents =
    List.filterMap
        toPoint2D
        (accidents |> filterByTimestamp model |> sortByTimestamp)


aspectRatioSelectorOption : Model -> AspectRatio -> Html Msg
aspectRatioSelectorOption model aspectRatio =
    let
        name =
            case aspectRatio of
                AspectRatioBanking45 ->
                    "Banking to 45 degrees"

                AspectRatio16to9 ->
                    "16:9"

                AspectRatio2to1 ->
                    "2:1"

                AspectRatioSquare ->
                    "1:1"
    in
    option
        [ onClick (SelectAspectRatio aspectRatio)
        , selected (model.aspectRatio == aspectRatio)
        ]
        [ text name ]


aspectRatioSelector : Model -> Html Msg
aspectRatioSelector model =
    let
        viewId =
            "aspect-ratio-selector"

        option =
            aspectRatioSelectorOption model

        options =
            List.map
                option
                [ AspectRatioBanking45
                , AspectRatioSquare
                , AspectRatio2to1
                , AspectRatio16to9
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Choose an aspect ratio: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


dimensionReferenceLabel : DimensionReference -> String
dimensionReferenceLabel dimensionReference =
    case dimensionReference of
        DimensionReferenceAbsolute ->
            "absolute"

        DimensionReferenceRelative ->
            "relative"


dimensionYLabel : DimensionY -> String
dimensionYLabel dimensionY =
    case dimensionY of
        DimensionYInjured reference ->
            "Injured (" ++ dimensionReferenceLabel reference ++ ")"

        DimensionYKilled reference ->
            "Killed (" ++ dimensionReferenceLabel reference ++ ")"


dimensionYSelectorOption : Model -> DimensionY -> Html Msg
dimensionYSelectorOption model dimensionY =
    option
        [ onClick (SelectDimensionY dimensionY)
        , selected (model.dimensionY == dimensionY)
        ]
        [ text (dimensionYLabel dimensionY) ]


dimensionYSelector : Model -> Html Msg
dimensionYSelector model =
    let
        viewId =
            "dimension-y-selector"

        option =
            dimensionYSelectorOption model

        options =
            List.map
                option
                [ DimensionYInjured DimensionReferenceAbsolute
                , DimensionYKilled DimensionReferenceAbsolute
                , DimensionYInjured DimensionReferenceRelative
                , DimensionYKilled DimensionReferenceRelative
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Choose an Y dimension: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


view : Model -> List Accident -> Html Msg
view model accidents =
    let
        points =
            toPoints2D model accidents

        data =
            AxisData2D "Time" "Road Width" points

        aspectRatio : Float
        aspectRatio =
            computeAspectRatio model points
    in
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        , aspectRatioSelector model
        , dimensionYSelector model
        , h3 []
            [ text ("Aspect Ratio: " ++ String.fromFloat aspectRatio)
            ]
        , fromUnstyled
            (linePlot aspectRatio data)
        ]


mapConsecutive : (a -> a -> b) -> List a -> Maybe (List b)
mapConsecutive mapper list =
    Maybe.map (\l2 -> List.map2 mapper list l2) <| List.tail list


computeAspectRatio : Model -> List Point2D -> Float
computeAspectRatio model data =
    case model.aspectRatio of
        AspectRatioSquare ->
            1 / 1

        AspectRatio2to1 ->
            2 / 1

        AspectRatio16to9 ->
            16 / 9

        AspectRatioBanking45 ->
            Maybe.withDefault 1 (computeAspectRatioBanking45 data)


computeAspectRatioBanking45 : List Point2D -> Maybe Float
computeAspectRatioBanking45 data =
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


linePlot : Float -> AxisData2D -> Svg msg
linePlot aspectRatio model =
    let
        width : Float
        width =
            900

        height : Float
        height =
            width / aspectRatio

        padding : Float
        padding =
            60

        xTicks : Int
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
