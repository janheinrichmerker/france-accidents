module Visualization2 exposing (..)

import Axis
import Color exposing (black)
import Dict
import Html.Styled exposing (Html, br, div, form, fromUnstyled, option, select, text)
import Html.Styled.Attributes exposing (selected)
import Html.Styled.Events exposing (onClick)
import List.Statistics
import Model exposing (Accident)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, g, image, svg, text_)
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


type alias Coordinates =
    ( Float, Float )


type alias CoordinatesRange =
    ( Coordinates, Coordinates )


type alias CoordinatesPoint =
    ( Coordinates, List Float )


type alias CoordinatesData =
    List ( Coordinates, List (List Float) )


type alias Model =
    { backgroundUrl : String
    , boundaries : CoordinatesRange
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
      , boundaries = ( ( 51.5, -5.8 ), ( 41, 10 ) )
      , group = GroupNone
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


toCoordinates : Accident -> Maybe Coordinates
toCoordinates accident =
    Maybe.map2 Tuple.pair accident.latitude accident.longitude


isInBounds : CoordinatesRange -> Coordinates -> Bool
isInBounds ( ( minLat, minLong ), ( maxLat, maxLong ) ) ( lat, long ) =
    isBetween minLat maxLat lat && isBetween minLong maxLong long


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


latitudeScale : CoordinatesRange -> ( Float, Float ) -> ContinuousScale Float
latitudeScale ( ( min, _ ), ( max, _ ) ) =
    Scale.linear ( min, max )


longitudeScale : CoordinatesRange -> ( Float, Float ) -> ContinuousScale Float
longitudeScale ( ( _, min ), ( _, max ) ) =
    Scale.linear ( min, max )


toGridGroupKey : Int -> Int -> CoordinatesRange -> Coordinates -> Int
toGridGroupKey cols rows bounds ( lat, long ) =
    let
        scaleLat : ContinuousScale Float
        scaleLat =
            latitudeScale bounds ( 0, toFloat rows )

        scaleLong : ContinuousScale Float
        scaleLong =
            longitudeScale bounds ( 0, toFloat cols )

        row : Int
        row =
            Scale.convert scaleLat lat |> floor

        col : Int
        col =
            Scale.convert scaleLong long |> floor
    in
    row + col * rows


toGroupKey : Group -> CoordinatesRange -> Accident -> Int
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


associateGroupKey : Group -> CoordinatesRange -> Accident -> ( Int, Accident )
associateGroupKey group bounds accident =
    ( toGroupKey group bounds accident, accident )


groupBy : Group -> CoordinatesRange -> List Accident -> List (List Accident)
groupBy group bounds accidents =
    accidents
        |> List.map (associateGroupKey group bounds)
        |> toBucketDict
        |> Dict.values


groupCoordinates : List Accident -> Maybe Coordinates
groupCoordinates group =
    let
        coordinates : List Coordinates
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


associateGroupCoordinates : List (List Accident) -> List ( List Accident, Coordinates )
associateGroupCoordinates groups =
    let
        associate : List Accident -> Maybe ( List Accident, Coordinates )
        associate group =
            Maybe.map (Tuple.pair group) (groupCoordinates group)
    in
    groups |> List.filterMap associate


accidentData : Accident -> List Float
accidentData accident =
    -- todo
    []


accidentsData : List Accident -> List (List Float)
accidentsData accidents =
    accidents |> List.map accidentData


groupedCoordinatePoints : Group -> CoordinatesRange -> List Accident -> List ( Coordinates, List (List Float) )
groupedCoordinatePoints group bounds accidents =
    let
        mapAccidentData : ( List Accident, Coordinates ) -> ( List (List Float), Coordinates )
        mapAccidentData accidentCoordinates =
            accidentCoordinates |> Tuple.mapFirst accidentsData
    in
    accidents
        |> groupBy group bounds
        |> associateGroupCoordinates
        |> List.map mapAccidentData
        |> List.map reverseTuple


marker : List Float -> Svg msg
marker _ =
    circle
        [ TypedSvg.Attributes.r (Px 2)
        , TypedSvg.Attributes.fill (Paint black)
        , TypedSvg.Attributes.fillOpacity (Opacity 0.25)
        , TypedSvg.Attributes.stroke (Paint black)
        ]
        []


markers : List (List Float) -> Svg msg
markers data =
    let
        markersList : List (Svg msg)
        markersList =
            data |> List.map marker
    in
    g
        []
        (markersList
            ++ [ text_
                    [ TypedSvg.Attributes.x (Px 0)
                    , TypedSvg.Attributes.y (Px 0)
                    , TypedSvg.Attributes.textAnchor AnchorMiddle
                    ]
                    [ -- todo
                      TypedSvg.Core.text "Point"
                    ]
               ]
        )


point : ContinuousScale Float -> ContinuousScale Float -> ( Coordinates, List (List Float) ) -> Svg msg
point scaleX scaleY ( coordinates, data ) =
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
        [ markers data
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
                |> filterByBounds model.boundaries
                |> groupedCoordinatePoints model.group model.boundaries
    in
    div
        []
        [ groupSelector model
        , displaySelector model
        , text (String.fromInt (List.length accidents))
        , points
            |> scatterplot model.backgroundUrl model.boundaries model.display
        ]
