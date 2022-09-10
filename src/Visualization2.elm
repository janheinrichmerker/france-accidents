module Visualization2 exposing (..)

import Axis
import Color exposing (black)
import Dict
import Html.Styled exposing (Html, div, form, fromUnstyled, option, select, text)
import Html.Styled.Attributes exposing (selected)
import Html.Styled.Events exposing (onClick)
import List.Extra
import List.Statistics
import Maybe.Extra
import Model exposing (Accident, Person, TravelReason(..), Vehicle)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (g, image, line, svg, text_)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Align(..), AnchorAlignment(..), Length(..), MeetOrSlice(..), Opacity(..), Paint(..), Transform(..))
import Utils exposing (hashString, isBetween, reverseTuple, toBucketDict, tupleMean)


type Group
    = GroupNone
      -- With the number of columns and rows to group into.
    | GroupCoordinates Int Int
    | GroupDepartments
    | GroupCommunes


type Display
    = DisplayAverage
    | DisplayXray


type alias GeoCoordinates =
    ( Float, Float )


type alias GeoCoordinatesBounds =
    ( GeoCoordinates, GeoCoordinates )


type alias GeoData =
    List ( GeoCoordinates, List StickFigureData )


type StickFigureData
    = StickFigureData Float Float Float Float Float


type alias Model =
    { backgroundUrl : String
    , bounds : GeoCoordinatesBounds
    , group : Group
    , display : Display
    }


type Msg
    = NoOp
    | SelectGroup Group
    | SelectDisplay Display


label : String
label =
    "Person Characteristics Stick Figures"


init : ( Model, Cmd Msg )
init =
    ( { backgroundUrl = "/france.svg"
      , bounds = ( ( 51.5, -5.8 ), ( 41, 10 ) )
      , group = GroupDepartments
      , display = DisplayAverage
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectGroup group ->
            ( { model | group = group }, Cmd.none )

        SelectDisplay display ->
            ( { model | display = display }, Cmd.none )


toCoordinates : Accident -> Maybe GeoCoordinates
toCoordinates accident =
    Maybe.map2 Tuple.pair accident.latitude accident.longitude


isInBounds : GeoCoordinatesBounds -> GeoCoordinates -> Bool
isInBounds ( ( minLat, minLong ), ( maxLat, maxLong ) ) ( lat, long ) =
    isBetween minLat maxLat lat && isBetween minLong maxLong long


filterByBounds : GeoCoordinatesBounds -> List Accident -> List Accident
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


toGridGroupKey : Int -> Int -> GeoCoordinatesBounds -> GeoCoordinates -> Int
toGridGroupKey cols rows ( ( minLat, minLong ), ( maxLat, maxLong ) ) ( lat, long ) =
    let
        scaleLat : ContinuousScale Float
        scaleLat =
            Scale.linear ( 0, toFloat rows ) ( minLat, maxLat )

        scaleLong : ContinuousScale Float
        scaleLong =
            Scale.linear ( 0, toFloat cols ) ( minLong, maxLong )

        row : Int
        row =
            Scale.convert scaleLat lat |> floor

        col : Int
        col =
            Scale.convert scaleLong long |> floor
    in
    row + col * rows


toGroupKey : Group -> GeoCoordinatesBounds -> Accident -> Int
toGroupKey group bounds accident =
    case group of
        GroupNone ->
            accident.accident_id

        GroupCoordinates cols rows ->
            accident
                |> toCoordinates
                |> Maybe.map (toGridGroupKey cols rows bounds)
                |> Maybe.withDefault -1

        GroupDepartments ->
            accident.department |> hashString

        GroupCommunes ->
            accident.department ++ accident.commune |> hashString


associateGroupKey : Group -> GeoCoordinatesBounds -> Accident -> ( Int, Accident )
associateGroupKey group bounds accident =
    ( toGroupKey group bounds accident, accident )


groupBy : Group -> GeoCoordinatesBounds -> List Accident -> List (List Accident)
groupBy group bounds accidents =
    accidents
        |> List.map (associateGroupKey group bounds)
        |> toBucketDict
        |> Dict.values


groupCoordinates : List Accident -> Maybe GeoCoordinates
groupCoordinates group =
    let
        coordinates : List GeoCoordinates
        coordinates =
            group |> List.filterMap toCoordinates

        ( latitudes, longitudes ) =
            coordinates |> List.unzip

        latitude : Maybe Float
        latitude =
            latitudes |> List.Statistics.mean

        longitude : Maybe Float
        longitude =
            longitudes |> List.Statistics.mean
    in
    Maybe.map2 Tuple.pair latitude longitude


associateGroupCoordinates : List (List Accident) -> List ( List Accident, GeoCoordinates )
associateGroupCoordinates groups =
    let
        associate : List Accident -> Maybe ( List Accident, GeoCoordinates )
        associate group =
            Maybe.map (Tuple.pair group) (groupCoordinates group)
    in
    groups |> List.filterMap associate


listToStickFigureData : List Float -> Maybe StickFigureData
listToStickFigureData values =
    case values of
        m1 :: m2 :: m3 :: m4 :: m5 :: [] ->
            StickFigureData m1 m2 m3 m4 m5 |> Just

        _ ->
            Nothing


combineStickFigureData : List (Maybe Float) -> Maybe StickFigureData
combineStickFigureData values =
    values |> Maybe.Extra.combine |> Maybe.andThen listToStickFigureData


personSex : Accident -> Vehicle -> Person -> Maybe Float
personSex _ _ person =
    case person.sex of
        Model.SexFemale ->
            Just 1

        Model.SexMale ->
            Just 0


personBirthYear : Accident -> Vehicle -> Person -> Maybe Float
personBirthYear _ _ person =
    -- todo normalize to 0-1
    Maybe.map toFloat person.birth_year


personReason : Accident -> Vehicle -> Person -> Maybe Float
personReason _ _ person =
    person.travel_reason
        |> Maybe.map
            (\reason ->
                case reason of
                    TravelReasonProfessional ->
                        1

                    TravelReasonHomeToWork ->
                        0.8

                    TravelReasonHomeToSchool ->
                        0.6

                    TravelReasonShopping ->
                        0.4

                    TravelReasonWalkingLeisure ->
                        0.2

                    TravelReasonOther ->
                        0
            )


personSafetyEquipment : Accident -> Vehicle -> Person -> Maybe Float
personSafetyEquipment _ _ person =
    let
        numSafetyEquipments =
            List.length person.safety_equipment
    in
    Just (1 / toFloat numSafetyEquipments)


vehicleLoneliness : Accident -> Vehicle -> Person -> Maybe Float
vehicleLoneliness _ vehicle _ =
    let
        numPersons =
            List.length vehicle.persons
    in
    Just (1 / toFloat numPersons)


personToStickFigureData : Accident -> Vehicle -> Person -> Maybe StickFigureData
personToStickFigureData accident vehicle person =
    let
        features : List (Accident -> Vehicle -> Person -> Maybe Float)
        features =
            [ personSex
            , personBirthYear
            , personReason
            , personSafetyEquipment
            , vehicleLoneliness
            ]
    in
    features
        |> List.map (\feature -> feature accident vehicle person)
        |> combineStickFigureData


vehicleToStickFigureData : Accident -> Vehicle -> List StickFigureData
vehicleToStickFigureData accident vehicle =
    vehicle.persons |> List.filterMap (personToStickFigureData accident vehicle)


accidentToStickFigureData : Accident -> List StickFigureData
accidentToStickFigureData accident =
    accident.vehicles |> List.concatMap (vehicleToStickFigureData accident)


accidentsToStickFigureData : List Accident -> List StickFigureData
accidentsToStickFigureData accidents =
    accidents |> List.concatMap accidentToStickFigureData


groupedCoordinatePoints : Group -> GeoCoordinatesBounds -> List Accident -> List ( GeoCoordinates, List StickFigureData )
groupedCoordinatePoints group bounds accidents =
    let
        mapAccidentData : ( List Accident, GeoCoordinates ) -> ( List StickFigureData, GeoCoordinates )
        mapAccidentData accidentCoordinates =
            accidentCoordinates
                |> Tuple.mapFirst accidentsToStickFigureData
    in
    accidents
        |> groupBy group bounds
        |> associateGroupCoordinates
        |> List.map mapAccidentData
        |> List.map reverseTuple


stickFigureHelp : Float -> Float -> Float -> Float -> Float -> Float -> Svg msg
stickFigureHelp opacity v1 v2 v3 v4 v5 =
    let
        width : Float
        width =
            6

        scaleAlpha : ContinuousScale Float
        scaleAlpha =
            Scale.linear ( 0, 180 ) ( 0, 1 )

        alpha : Float
        alpha =
            Scale.convert scaleAlpha v1

        scaleBeta : ContinuousScale Float
        scaleBeta =
            Scale.linear ( 0, 90 ) ( 0, 1 )

        beta : Float
        beta =
            Scale.convert scaleBeta v2

        scaleGamma : ContinuousScale Float
        scaleGamma =
            Scale.linear ( 0, -90 ) ( 0, 1 )

        gamma : Float
        gamma =
            Scale.convert scaleGamma v3

        scaleDelta : ContinuousScale Float
        scaleDelta =
            Scale.linear ( 0, 90 ) ( 0, 1 )

        delta : Float
        delta =
            Scale.convert scaleDelta v4

        scaleEpsilon : ContinuousScale Float
        scaleEpsilon =
            Scale.linear ( 0, -90 ) ( 0, 1 )

        epsilon : Float
        epsilon =
            Scale.convert scaleEpsilon v5
    in
    g
        [ TypedSvg.Attributes.opacity (Opacity opacity)
        , TypedSvg.Attributes.transform [ Rotate alpha 0 0 ]
        ]
        [ line
            [ TypedSvg.Attributes.x1 (Px 0)
            , TypedSvg.Attributes.y1 (Px -(width / 2))
            , TypedSvg.Attributes.x2 (Px 0)
            , TypedSvg.Attributes.y2 (Px (width / 2))
            , TypedSvg.Attributes.stroke (Paint black)
            , TypedSvg.Attributes.strokeWidth (Px 1)
            ]
            []
        , g
            [ TypedSvg.Attributes.transform [ Translate 0 -(width / 2) ] ]
            [ line
                [ TypedSvg.Attributes.x1 (Px 0)
                , TypedSvg.Attributes.y1 (Px 0)
                , TypedSvg.Attributes.x2 (Px 0)
                , TypedSvg.Attributes.y2 (Px -width)
                , TypedSvg.Attributes.stroke (Paint black)
                , TypedSvg.Attributes.strokeWidth (Px 1)
                , TypedSvg.Attributes.transform [ Rotate beta 0 0 ]
                ]
                []
            , line
                [ TypedSvg.Attributes.x1 (Px 0)
                , TypedSvg.Attributes.y1 (Px 0)
                , TypedSvg.Attributes.x2 (Px 0)
                , TypedSvg.Attributes.y2 (Px -width)
                , TypedSvg.Attributes.stroke (Paint black)
                , TypedSvg.Attributes.strokeWidth (Px 1)
                , TypedSvg.Attributes.transform [ Rotate gamma 0 0 ]
                ]
                []
            ]
        , g
            [ TypedSvg.Attributes.transform [ Translate 0 (width / 2) ] ]
            [ line
                [ TypedSvg.Attributes.x1 (Px 0)
                , TypedSvg.Attributes.y1 (Px 0)
                , TypedSvg.Attributes.x2 (Px 0)
                , TypedSvg.Attributes.y2 (Px width)
                , TypedSvg.Attributes.stroke (Paint black)
                , TypedSvg.Attributes.strokeWidth (Px 1)
                , TypedSvg.Attributes.transform [ Rotate delta 0 0 ]
                ]
                []
            , line
                [ TypedSvg.Attributes.x1 (Px 0)
                , TypedSvg.Attributes.y1 (Px 0)
                , TypedSvg.Attributes.x2 (Px 0)
                , TypedSvg.Attributes.y2 (Px width)
                , TypedSvg.Attributes.stroke (Paint black)
                , TypedSvg.Attributes.strokeWidth (Px 1)
                , TypedSvg.Attributes.transform [ Rotate epsilon 0 0 ]
                ]
                []
            ]
        ]


stickFigure : Float -> StickFigureData -> Svg msg
stickFigure opacity data =
    case data of
        StickFigureData v1 v2 v3 v4 v5 ->
            stickFigureHelp opacity v1 v2 v3 v4 v5


stickFigureDataToList : StickFigureData -> List Float
stickFigureDataToList data =
    case data of
        StickFigureData v1 v2 v3 v4 v5 ->
            [ v1, v2, v3, v4, v5 ]


meanDimensions : List StickFigureData -> Maybe StickFigureData
meanDimensions points =
    let
        dimensionPoints : List (List Float)
        dimensionPoints =
            points |> List.map stickFigureDataToList |> List.Extra.transpose

        dimensionMeans : List (Maybe Float)
        dimensionMeans =
            dimensionPoints |> List.map List.Statistics.mean
    in
    dimensionMeans |> combineStickFigureData


stickFigureLabel : StickFigureData -> String
stickFigureLabel data =
    data
        |> stickFigureDataToList
        |> List.map String.fromFloat
        |> String.join ", "


markers : Display -> List StickFigureData -> List (Svg msg)
markers display data =
    case display of
        DisplayXray ->
            let
                opacity =
                    1 / toFloat (List.length data)
            in
            data
                |> List.map (stickFigure opacity)

        DisplayAverage ->
            data
                |> meanDimensions
                |> Maybe.map (stickFigure 1)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []


point : ContinuousScale Float -> ContinuousScale Float -> Display -> ( GeoCoordinates, List StickFigureData ) -> Svg msg
point scaleX scaleY display ( coordinates, data ) =
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
        (markers display data
            ++ [ text_
                    [ TypedSvg.Attributes.x (Px 0)
                    , TypedSvg.Attributes.y (Px 0)
                    , TypedSvg.Attributes.textAnchor AnchorMiddle
                    ]
                    [ data
                        |> meanDimensions
                        |> Maybe.map stickFigureLabel
                        |> Maybe.withDefault "no data"
                        |> TypedSvg.Core.text
                    ]
               ]
        )


scatterplot : String -> GeoCoordinatesBounds -> Display -> GeoData -> Html Msg
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
            (data |> List.map (point xScale yScale display))
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


displayLabel : Display -> String
displayLabel display =
    case display of
        DisplayAverage ->
            "average values"

        DisplayXray ->
            "x-ray"


displaySelectorOption : Model -> Display -> Html Msg
displaySelectorOption model display =
    option
        [ onClick (SelectDisplay display)
        , selected (model.display == display)
        ]
        [ text (displayLabel display) ]


displaySelector : Model -> Html Msg
displaySelector model =
    let
        viewId =
            "display-selector"

        option =
            displaySelectorOption model

        options =
            List.map
                option
                [ DisplayAverage
                , DisplayXray
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Display style: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


groupLabel : Group -> String
groupLabel group =
    case group of
        GroupNone ->
            "no grouping"

        GroupCoordinates cols rows ->
            "by grid (" ++ String.fromInt cols ++ "x" ++ String.fromInt rows ++ ")"

        GroupDepartments ->
            "by departements"

        GroupCommunes ->
            "by communes"


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
            "group-selector"

        option =
            groupSelectorOption model

        options =
            List.map
                option
                [ GroupNone
                , GroupCommunes
                , GroupDepartments
                , GroupCoordinates 10 10
                , GroupCoordinates 5 5
                , GroupCoordinates 3 3
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Group by: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


view : Model -> List Accident -> Html Msg
view model accidents =
    let
        points =
            accidents
                |> filterByBounds model.bounds
                |> groupedCoordinatePoints model.group model.bounds
    in
    div
        []
        [ groupSelector model
        , displaySelector model
        , text (String.fromInt (List.length accidents))
        , points
            |> scatterplot model.backgroundUrl model.bounds model.display
        ]
