module Visualization1 exposing (Model, Msg(..), init, label, update, view)

import Axis
import Color exposing (black)
import Dict
import Html.Styled exposing (Html, button, div, form, fromUnstyled, h3, option, select, text)
import Html.Styled.Attributes exposing (selected)
import Html.Styled.Events exposing (onClick)
import List.Extra
import Model exposing (Accident, Person, Severity(..), Vehicle)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import Task exposing (perform)
import Time exposing (Posix, millisToPosix, now, posixToMillis)
import Time.DateTime
import TimeUtils exposing (removeYear, retainMonth, retainQuarter, retainWeek, retainYear)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Align(..), Length(..), MeetOrSlice(..), Opacity(..), Paint(..), Scale(..), Transform(..))
import Utils exposing (mapConsecutive, reverseTuple, toBucketDict)


type AspectRatio
    = AspectRatioSquare
    | AspectRatio2To1
    | AspectRatio16To9
    | AspectRatioBanking45


type Reference
    = ReferenceRelative
    | ReferenceAbsolute


type Dimension
    = DimensionUnharmedPersons
    | DimensionInjuredPersons
    | DimensionKilledPersons
    | DimensionKilledOrInjuredPersons
    | DimensionPersons


type Group
    = GroupNever
    | GroupByYear


type Aggregate
    = AggregateNone
    | AggregatePerWeek
    | AggregatePerMonth
    | AggregatePerQuarter
    | AggregatePerYear


type alias Model =
    { timestamp : Maybe Posix
    , aspectRatio : AspectRatio
    , dimension : Dimension
    , reference : Reference
    , group : Group
    , aggregate : Aggregate
    }


type Msg
    = SetGlobalFilteredAccidents (List Accident) String
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
      , dimension = DimensionKilledOrInjuredPersons
      , reference = ReferenceAbsolute
      , group = GroupByYear
      , aggregate = AggregatePerMonth
      }
    , getTime
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        SetGlobalFilteredAccidents _ _ ->
            -- Handled in Main module.
            ( model, Cmd.none )


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


computeDimension : Model -> List Accident -> Float
computeDimension model accidents =
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

                DimensionKilledOrInjuredPersons ->
                    \p -> isKilled p || isInjured p

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
    toFloat numberPersonsFiltered / toFloat discriminator


toTimePoint : Model -> ( Posix, List Accident ) -> TimePoint
toTimePoint model ( timestamp, accidents ) =
    let
        y : Float
        y =
            computeDimension model accidents
    in
    ( timestamp, y )


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

        AggregatePerQuarter ->
            retainQuarter timestamp

        AggregatePerYear ->
            retainYear timestamp


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


toBuckets : Model -> List Accident -> List ( Posix, List Accident )
toBuckets model accidents =
    accidents
        |> filterByTimestamp model
        |> bucketsByTimestamp model
        |> sortByBucketTimestamp


toPoints2D : Model -> List Accident -> List TimePoint
toPoints2D model accidents =
    accidents
        |> toBuckets model
        |> List.map (toTimePoint model)


filteredAccidents : Model -> List Accident -> List Accident
filteredAccidents model accidents =
    accidents
        |> toBuckets model
        |> List.map Tuple.second
        |> List.concat


aspectRatioSelectorOption : Model -> AspectRatio -> Html Msg
aspectRatioSelectorOption model aspectRatio =
    let
        name : String
        name =
            case aspectRatio of
                AspectRatioBanking45 ->
                    "Banking to 45 degrees"

                AspectRatio16To9 ->
                    "16:9"

                AspectRatio2To1 ->
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
        viewId : String
        viewId =
            "aspect-ratio-selector"

        option : AspectRatio -> Html Msg
        option =
            aspectRatioSelectorOption model

        options : List (Html Msg)
        options =
            List.map
                option
                [ AspectRatioBanking45
                , AspectRatioSquare
                , AspectRatio2To1
                , AspectRatio16To9
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

        DimensionKilledOrInjuredPersons ->
            "killed or injured persons"


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
        viewId : String
        viewId =
            "dimension-selector"

        option : Dimension -> Html Msg
        option =
            dimensionSelectorOption model

        options : List (Html Msg)
        options =
            List.map
                option
                [ DimensionKilledOrInjuredPersons
                , DimensionKilledPersons
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
        viewId : String
        viewId =
            "reference-selector"

        option : Reference -> Html Msg
        option =
            referenceSelectorOption model

        options : List (Html Msg)
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
        viewId : String
        viewId =
            "group-x-selector"

        option : Group -> Html Msg
        option =
            groupSelectorOption model

        options : List (Html Msg)
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

        AggregatePerQuarter ->
            "per quarter"

        AggregatePerYear ->
            "per year"


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
        viewId : String
        viewId =
            "aggregate-x-selector"

        option : Aggregate -> Html Msg
        option =
            aggregateSelectorOption model

        options : List (Html Msg)
        options =
            List.map
                option
                [ AggregateNone
                , AggregatePerWeek
                , AggregatePerMonth
                , AggregatePerQuarter
                , AggregatePerYear
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
        filtered : List Accident
        filtered =
            filteredAccidents model accidents

        points : List TimePoint
        points =
            toPoints2D model accidents

        dataLabel : String
        dataLabel =
            dimensionLabel model.dimension ++ " (" ++ referenceLabel model.reference ++ ")"

        data : TimeSeriesData
        data =
            TimeSeriesData dataLabel points

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
        , button
            [ onClick (SetGlobalFilteredAccidents filtered "with time") ]
            [ text "globally filter accidents with time" ]
        , h3 []
            [ text ("Aspect Ratio: " ++ String.fromFloat aspectRatio)
            ]
        , fromUnstyled
            (timeSeriesLinePlot aspectRatio model.group data)
        ]


computeAspectRatio : Model -> List TimePoint -> Float
computeAspectRatio model data =
    case model.aspectRatio of
        AspectRatioSquare ->
            1 / 1

        AspectRatio2To1 ->
            2 / 1

        AspectRatio16To9 ->
            16 / 9

        AspectRatioBanking45 ->
            Maybe.withDefault 1 (computeAspectRatioBanking45 data)


computeAspectRatioBanking45 : List TimePoint -> Maybe Float
computeAspectRatioBanking45 data =
    let
        points : List ( Float, Float )
        points =
            List.map timePointCoordinates data

        slopes : Maybe (List Float)
        slopes =
            mapConsecutive (\( aX, aY ) ( bX, bY ) -> abs ((bY - aY) / (bX - aX))) points

        -- Calculate the Median.
        medianSlope : Maybe Float
        medianSlope =
            Maybe.andThen
                (\justSlopes -> Statistics.quantile 0.5 (List.sort justSlopes))
                slopes

        -- Calculate the range of x values, i.e., min and max.
        rangeX : Maybe ( Float, Float )
        rangeX =
            points
                |> List.map Tuple.first
                |> Statistics.extent

        -- Calculate the range of y values, i.e., min and max.
        rangeY : Maybe ( Float, Float )
        rangeY =
            points
                |> List.map Tuple.second
                |> Statistics.extent
    in
    Maybe.map3
        (\sm ( xmin, xmax ) ( ymin, ymax ) ->
            -- Calculate the aspect ratio
            sm * (xmax - xmin) / (ymax - ymin)
        )
        medianSlope
        rangeX
        rangeY


intToStringLeadingZeros : Int -> Int -> String
intToStringLeadingZeros pad int =
    let
        string : String
        string =
            String.fromInt int

        stringLength : Int
        stringLength =
            String.length string

        zeros : String
        zeros =
            String.repeat (pad - stringLength) "0"
    in
    zeros ++ string


timeSeriesTickLabel : Group -> Float -> String
timeSeriesTickLabel group time =
    let
        date : Time.DateTime.DateTime
        date =
            time |> round |> millisToPosix |> Time.DateTime.fromPosix
    in
    case group of
        GroupNever ->
            String.join "-"
                [ date |> Time.DateTime.year |> intToStringLeadingZeros 4
                , date |> Time.DateTime.month |> intToStringLeadingZeros 2
                , date |> Time.DateTime.day |> intToStringLeadingZeros 2
                ]

        GroupByYear ->
            String.join "-"
                [ date |> Time.DateTime.month |> intToStringLeadingZeros 2
                , date |> Time.DateTime.day |> intToStringLeadingZeros 2
                ]


timeSeriesLinePlot : Float -> Group -> TimeSeriesData -> Svg msg
timeSeriesLinePlot aspectRatio group model =
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
            6

        data : List TimePoint
        data =
            model.data

        xScale : ContinuousScale Float
        xScale =
            data
                |> List.map Tuple.first
                |> List.map posixToMillis
                |> List.map toFloat
                |> Statistics.extent
                |> Maybe.withDefault ( 0, 0 )
                |> Scale.linear ( 0, width )

        yTicks : Int
        yTicks =
            max 2 (round (10 / aspectRatio))

        yScale : ContinuousScale Float
        yScale =
            data
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\b -> ( 0, b ))
                |> Scale.linear ( height, 0 )
                |> Scale.nice yTicks

        convertScales : ( Float, Float ) -> ( Float, Float )
        convertScales ( x, y ) =
            ( Scale.convert xScale x, Scale.convert yScale y )

        topLineGenerator : List ( Float, Float )
        topLineGenerator =
            data
                |> List.map timePointCoordinates
                |> List.map convertScales

        bottomLineGeneratorLine : List ( Float, Float )
        bottomLineGeneratorLine =
            data
                |> List.map (\( x, _ ) -> ( x, 0 ))
                |> List.map timePointCoordinates
                |> List.map convertScales

        topLine : Path
        topLine =
            topLineGenerator
                |> List.map Just
                |> Shape.line Shape.monotoneInXCurve

        area : Path
        area =
            topLineGenerator
                |> List.Extra.zip bottomLineGeneratorLine
                |> List.map Just
                |> Shape.area Shape.monotoneInXCurve
    in
    svg
        [ TypedSvg.Attributes.viewBox 0 0 (width + 2 * padding) (height + 2 * padding)
        , TypedSvg.Attributes.width (Percent 50)
        , TypedSvg.Attributes.height (Percent 50)
        , TypedSvg.Attributes.preserveAspectRatio (Align ScaleMin ScaleMin) Slice
        ]
        [ g [ TypedSvg.Attributes.transform [ Translate (padding - 1) (height + padding) ] ]
            [ Axis.bottom
                [ Axis.tickCount xTicks
                , Axis.tickFormat (timeSeriesTickLabel group)
                ]
                xScale
            ]
        , g [ TypedSvg.Attributes.transform [ Translate (padding - 1) padding ] ]
            [ Axis.left
                [ Axis.tickCount yTicks ]
                yScale
            , text_
                [ TypedSvg.Attributes.fontFamily [ "sans-serif" ]
                , TypedSvg.Attributes.fontSize (Px 10)
                , TypedSvg.Attributes.x (Px 5)
                , TypedSvg.Attributes.y (Px 5)
                ]
                [ TypedSvg.Core.text model.description ]
            ]
        , g
            [ TypedSvg.Attributes.transform [ Translate padding padding ]
            , TypedSvg.Attributes.class [ "series" ]
            ]
            [ Path.element topLine
                [ TypedSvg.Attributes.stroke (Paint black)
                , TypedSvg.Attributes.strokeWidth (Px 1)
                , TypedSvg.Attributes.fill PaintNone
                ]
            , Path.element area
                [ TypedSvg.Attributes.fill (Paint black)
                , TypedSvg.Attributes.fillOpacity (Opacity 0.1)
                ]
            ]
        ]


type alias Point =
    ( Float, Float )


type alias TimePoint =
    ( Posix, Float )


timePointCoordinates : TimePoint -> ( Float, Float )
timePointCoordinates ( x, y ) =
    ( x |> posixToMillis |> toFloat, y )


type alias TimeSeriesData =
    { description : String
    , data : List TimePoint
    }
