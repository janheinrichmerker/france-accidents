module Visualization1 exposing (..)

import Axis
import Color exposing (black)
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, form, fromUnstyled, h3, option, select, text)
import Html.Styled.Attributes exposing (selected)
import Html.Styled.Events exposing (onClick)
import List.Extra
import Model exposing (Accident, Person, Severity(..), Vehicle)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Statistics exposing (extent, quantile)
import Task exposing (perform)
import Time exposing (Posix, millisToPosix, now, posixToMillis)
import TimeUtils exposing (removeYear, retainMonth, retainWeek)
import TypedSvg exposing (g, line, svg, text_)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Align(..), AnchorAlignment(..), Length(..), MeetOrSlice(..), Paint(..), Scale(..), Transform(..))
import Utils exposing (mapConsecutive, reverseTuple, toBucketDict)


type AspectRatio
    = AspectRatioSquare
    | AspectRatio2to1
    | AspectRatio16to9
    | AspectRatioBanking45


type Reference
    = ReferenceRelative
    | ReferenceAbsolute


type Dimension
    = DimensionUnharmedPersons
    | DimensionInjuredPersons
    | DimensionKilledPersons
    | DimensionPersons


type Group
    = GroupNever
    | GroupByYear


type Aggregate
    = AggregateNone
    | AggregatePerWeek
    | AggregatePerMonth


type alias Model =
    { timestamp : Maybe Posix
    , aspectRatio : AspectRatio
    , dimension : Dimension
    , reference : Reference
    , group : Group
    , aggregate : Aggregate
    }


type Msg
    = NoOp
    | GotTime Posix
    | SelectAspectRatio AspectRatio
    | SelectDimension Dimension
    | SelectReference Reference
    | SelectGroup Group
    | SelectAggregate Aggregate


label : String
label =
    "Severity Time Series"


init : ( Model, Cmd Msg )
init =
    ( { timestamp = Nothing
      , aspectRatio = AspectRatioBanking45
      , dimension = DimensionInjuredPersons
      , reference = ReferenceAbsolute
      , group = GroupByYear
      , aggregate = AggregatePerWeek
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

        SelectDimension dimension ->
            ( { model | dimension = dimension }, Cmd.none )

        SelectReference reference ->
            ( { model | reference = reference }, Cmd.none )

        SelectGroup groupX ->
            ( { model | group = groupX }, Cmd.none )

        SelectAggregate aggregateX ->
            ( { model | aggregate = aggregateX }, Cmd.none )


getTime : Cmd Msg
getTime =
    perform GotTime now


isUnharmed : Person -> Bool
isUnharmed person =
    case person.severity of
        SeverityUnharmed ->
            True

        _ ->
            False


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


computeDimensionX : Posix -> Maybe Float
computeDimensionX timestamp =
    Just (toFloat (posixToMillis timestamp))


computeDimensionY : Model -> List Accident -> Maybe Float
computeDimensionY model accidents =
    let
        vehicles : List Vehicle
        vehicles =
            List.foldl (\accident agg -> List.append agg accident.vehicles) [] accidents

        persons : List Person
        persons =
            List.foldl (\vehicle agg -> List.append agg vehicle.persons) [] vehicles

        filter : Person -> Bool
        filter =
            case model.dimension of
                DimensionUnharmedPersons ->
                    isUnharmed

                DimensionInjuredPersons ->
                    isInjured

                DimensionKilledPersons ->
                    isKilled

                DimensionPersons ->
                    \_ -> True

        numberPersonsFiltered : Int
        numberPersonsFiltered =
            List.Extra.count filter persons

        discriminator : Int
        discriminator =
            case model.reference of
                ReferenceAbsolute ->
                    1

                ReferenceRelative ->
                    List.length persons
    in
    Just (toFloat numberPersonsFiltered / toFloat discriminator)


toPoint2D : Model -> ( Posix, List Accident ) -> Maybe Point2D
toPoint2D model ( timestamp, accidents ) =
    let
        x =
            computeDimensionX timestamp

        y =
            computeDimensionY model accidents
    in
    Maybe.map2
        (\justX justY -> Point2D "" justX justY)
        x
        y


{-| Remove accidents that have a timestamp in the future, i.e., an invalid timestamp.
-}
filterByTimestamp : Model -> List Accident -> List Accident
filterByTimestamp model accidents =
    case model.timestamp of
        Just posix ->
            List.filter (\accident -> posixToMillis accident.timestamp <= posixToMillis posix) accidents

        Nothing ->
            accidents


{-| Sort buckets in chronological order, i.e., by ascending timestamp.
-}
sortByBucketTimestamp : List ( Posix, List Accident ) -> List ( Posix, List Accident )
sortByBucketTimestamp buckets =
    List.sortBy (Tuple.first >> posixToMillis) buckets


toAggregatedTimestamp : Aggregate -> Posix -> Posix
toAggregatedTimestamp aggregateX timestamp =
    case aggregateX of
        AggregateNone ->
            timestamp

        AggregatePerWeek ->
            retainWeek timestamp

        AggregatePerMonth ->
            retainMonth timestamp


toGroupedTimestamp : Group -> Posix -> Posix
toGroupedTimestamp groupX timestamp =
    case groupX of
        GroupNever ->
            timestamp

        GroupByYear ->
            removeYear timestamp


toTimestampKey : Model -> Accident -> Posix
toTimestampKey model accident =
    accident.timestamp
        |> toAggregatedTimestamp model.aggregate
        |> toGroupedTimestamp model.group


associateTimestampKey : Model -> Accident -> ( Accident, Posix )
associateTimestampKey model accident =
    ( accident, toTimestampKey model accident )


bucketsByTimestamp : Model -> List Accident -> List ( Posix, List Accident )
bucketsByTimestamp model accidents =
    accidents
        |> List.map (associateTimestampKey model)
        |> List.map reverseTuple
        |> List.map (Tuple.mapFirst posixToMillis)
        |> toBucketDict
        |> Dict.toList
        |> List.map (Tuple.mapFirst millisToPosix)


toPoints2D : Model -> List Accident -> List Point2D
toPoints2D model accidents =
    List.filterMap
        (toPoint2D model)
        (accidents
            |> filterByTimestamp model
            |> bucketsByTimestamp model
            |> sortByBucketTimestamp
        )


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
            [ text "Aspect ratio: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


dimensionLabel : Dimension -> String
dimensionLabel dimension =
    case dimension of
        DimensionPersons ->
            "persons"

        DimensionUnharmedPersons ->
            "unharmed persons"

        DimensionInjuredPersons ->
            "injured persons"

        DimensionKilledPersons ->
            "killed persons"


dimensionSelectorOption : Model -> Dimension -> Html Msg
dimensionSelectorOption model dimension =
    option
        [ onClick (SelectDimension dimension)
        , selected (model.dimension == dimension)
        ]
        [ text (dimensionLabel dimension) ]


dimensionSelector : Model -> Html Msg
dimensionSelector model =
    let
        viewId =
            "dimension-selector"

        option =
            dimensionSelectorOption model

        options =
            List.map
                option
                [ DimensionKilledPersons
                , DimensionInjuredPersons
                , DimensionUnharmedPersons
                , DimensionPersons
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Dimension: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


referenceLabel : Reference -> String
referenceLabel dimensionReference =
    case dimensionReference of
        ReferenceAbsolute ->
            "absolute"

        ReferenceRelative ->
            "relative"


referenceSelectorOption : Model -> Reference -> Html Msg
referenceSelectorOption model reference =
    option
        [ onClick (SelectReference reference)
        , selected (model.reference == reference)
        ]
        [ text (referenceLabel reference) ]


referenceSelector : Model -> Html Msg
referenceSelector model =
    let
        viewId =
            "reference-selector"

        option =
            referenceSelectorOption model

        options =
            List.map
                option
                [ ReferenceAbsolute
                , ReferenceRelative
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Reference: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


groupLabel : Group -> String
groupLabel group =
    case group of
        GroupNever ->
            "never"

        GroupByYear ->
            "by year"


groupSelectorOption : Model -> Group -> Html Msg
groupSelectorOption model group =
    option
        [ onClick (SelectGroup group)
        , selected (model.group == group)
        ]
        [ text (groupLabel group) ]


groupSelector : Model -> Html Msg
groupSelector model =
    let
        viewId =
            "group-x-selector"

        option =
            groupSelectorOption model

        options =
            List.map
                option
                [ GroupNever
                , GroupByYear
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Group values: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


aggregateLabel : Aggregate -> String
aggregateLabel aggregate =
    case aggregate of
        AggregateNone ->
            "no aggregation"

        AggregatePerWeek ->
            "per week"

        AggregatePerMonth ->
            "per month"


aggregateSelectorOption : Model -> Aggregate -> Html Msg
aggregateSelectorOption model aggregate =
    option
        [ onClick (SelectAggregate aggregate)
        , selected (model.aggregate == aggregate)
        ]
        [ text (aggregateLabel aggregate) ]


aggregateSelector : Model -> Html Msg
aggregateSelector model =
    let
        viewId =
            "aggregate-x-selector"

        option =
            aggregateSelectorOption model

        options =
            List.map
                option
                [ AggregateNone
                , AggregatePerWeek
                , AggregatePerMonth
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Aggregate values: " ]
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
        [ aspectRatioSelector model
        , dimensionSelector model
        , referenceSelector model
        , groupSelector model
        , aggregateSelector model
        , h3 []
            [ text ("Aspect Ratio: " ++ String.fromFloat aspectRatio)
            ]
        , fromUnstyled
            (linePlot aspectRatio data)
        ]


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
