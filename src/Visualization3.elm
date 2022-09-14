module Visualization3 exposing (Model, Msg(..), init, label, update, view)

import Color exposing (black)
import Html.Styled exposing (Html, button, div, form, fromUnstyled, input, li, ol, option, select, span, text, ul)
import Html.Styled.Attributes exposing (checked, selected, type_)
import Html.Styled.Events exposing (onClick)
import List.Extra
import Model exposing (Accident, AtmosphericConditions(..), Collision(..), Curvature(..), DedicatedLane(..), Intersection(..), Light(..), LocationRegime(..), Profile(..), RoadCategory(..), TrafficRegime(..))
import Partition exposing (Partitioner, Partitioners, equalityPartitioner, maybeEqualityPartitioner, partitionTree)
import Reorderable exposing (Reorderable)
import Tree exposing (Tree)
import TreeDiagram
import TreeDiagram.Svg
import TreeUtils exposing (toTreeDiagram)
import TypedSvg exposing (svg)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (Align(..), AnchorAlignment(..), Length(..), MeetOrSlice(..), Opacity(..), Paint(..), Scale(..), Transform(..))


type TreeLayout
    = TreeLayoutGraph
    | TreeLayoutTreemap
    | TreeLayoutList


type Dimension
    = DimensionLight
    | DimensionIntersection
    | DimensionAtmosphericConditions
    | DimensionCollision
    | DimensionLocationRegime
    | DimensionRoadCategory
    | DimensionTrafficRegime
    | DimensionDedicatedLane
    | DimensionProfile
    | DimensionCurvature


type alias PartitionerDimension =
    ( Dimension, Partitioner Accident String )


type alias Model =
    { treeLayout : TreeLayout
    , enabledDimensions : Reorderable PartitionerDimension
    , disabledDimensions : List PartitionerDimension
    }


type MoveDirection
    = MoveDirectionUp
    | MoveDirectionDown


type Msg
    = SetGlobalFilteredAccidents (List Accident) String
    | SelectTreeLayout TreeLayout
    | MoveDimension Int MoveDirection
    | ToggleDimension Int Bool


label : String
label =
    "Accident Type Tree"


initEnabledDimensions : List PartitionerDimension
initEnabledDimensions =
    [ ( DimensionRoadCategory
      , equalityPartitioner .roadCategory
            [ ( RoadCategoryHighway, "highway" )
            , ( RoadCategoryNationalRoad, "national road" )
            , ( RoadCategoryDepartmentalRoad, "departmental road" )
            , ( RoadCategoryMunicipalRoads, "municipal road" )
            , ( RoadCategoryOffThePublicNetwork, "off network" )
            , ( RoadCategoryParkingLotOpenToPublicTraffic, "parking lot" )
            , ( RoadCategoryUrbanMetropolitanRoads, "urban road" )
            ]
      )
    , ( DimensionProfile
      , maybeEqualityPartitioner .profile
            [ ( ProfileFlat, "flat" )
            , ( ProfileSlope, "slope" )
            , ( ProfileTopOfHill, "top of hill" )
            , ( ProfileBottomOfHill, "bottom of hill" )
            ]
      )
    , ( DimensionCurvature
      , maybeEqualityPartitioner .curvature
            [ ( CurvatureStraight, "straight" )
            , ( CurvatureLeftHandCurve, "left" )
            , ( CurvatureRightHandCurve, "right" )
            , ( CurvatureSCurve, "s-curve" )
            ]
      )
    ]


initDisabledDimensions : List PartitionerDimension
initDisabledDimensions =
    [ ( DimensionLight
      , maybeEqualityPartitioner .light
            [ ( LightDaylight, "daylight" )
            , ( LightDuskOrDawn, "dusk/dawn" )
            , ( LightNightWithoutPublicLighting, "night no lighting" )
            , ( LightNightWithPublicLightingOff, "night lighting off" )
            , ( LightNightWithPublicLightingOn, "night lighting on" )
            ]
      )
    , ( DimensionIntersection
      , maybeEqualityPartitioner .intersection
            [ ( IntersectionOutOfIntersection, "no intersection" )
            , ( IntersectionXIntersection, "X" )
            , ( IntersectionTIntersection, "T" )
            , ( IntersectionYIntersection, "Y" )
            , ( IntersectionIntersectionWithMoreThan4Branches, "> 4 branches" )
            , ( IntersectionRoundabout, "roundabout" )
            , ( IntersectionLevelCrossing, "level crossing" )
            ]
      )
    , ( DimensionAtmosphericConditions
      , maybeEqualityPartitioner .atmosphericConditions
            [ ( AtmosphericConditionsNormal, "normal" )
            , ( AtmosphericConditionsLightRain, "light rain" )
            , ( AtmosphericConditionsHeavyRain, "heavy rain" )
            , ( AtmosphericConditionsSnowHail, "snow/hail" )
            , ( AtmosphericConditionsFogSmoke, "fog/smoke" )
            , ( AtmosphericConditionsStrongWindStorm, "strong wind/storm" )
            , ( AtmosphericConditionsDazzlingWeather, "dazzling" )
            , ( AtmosphericConditionsOvercastWeather, "overcast" )
            ]
      )
    , ( DimensionCollision
      , maybeEqualityPartitioner .collision
            [ ( CollisionTwoVehiclesFront, "2 vehicles front" )
            , ( CollisionTwoVehiclesFromTheRear, "2 vehicles rear" )
            , ( CollisionTwoVehiclesFromTheSide, "2 vehicles side" )
            , ( CollisionThreeOrMoreVehiclesInAChain, ">2 vehicles chain" )
            , ( CollisionThreeOrMoreVehiclesMultipleCollisions, ">2 vehicles multiple" )
            , ( CollisionWithoutCollision, "no collision" )
            ]
      )
    , ( DimensionLocationRegime
      , maybeEqualityPartitioner .location
            [ ( LocationRegimeOutOfTown, "out of town" )
            , ( LocationRegimeInBuiltUpAreas, "built-up area" )
            ]
      )
    , ( DimensionTrafficRegime
      , maybeEqualityPartitioner .trafficRegime
            [ ( TrafficRegimeOneWay, "one way" )
            , ( TrafficRegimeBidirectional, "bidirectional" )
            , ( TrafficRegimeWithSeparateLanes, "separate lanes" )
            ]
      )
    , ( DimensionDedicatedLane
      , maybeEqualityPartitioner .dedicatedLane
            [ ( DedicatedLaneNone, "no dedicated lane" )
            , ( DedicatedLaneBicyclePath, "bike path" )
            , ( DedicatedLaneCycleLane, "cycle lane" )
            , ( DedicatedLaneReservedLane, "reserved lane" )
            ]
      )
    ]


init : ( Model, Cmd Msg )
init =
    ( { treeLayout = TreeLayoutTreemap
      , enabledDimensions = Reorderable.fromList initEnabledDimensions
      , disabledDimensions = initDisabledDimensions
      }
    , Cmd.none
    )


partitioners : Model -> Partitioners Accident String
partitioners model =
    model.enabledDimensions
        |> Reorderable.toList
        |> List.map Tuple.second


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTreeLayout layout ->
            ( { model | treeLayout = layout }, Cmd.none )

        MoveDimension idx MoveDirectionUp ->
            ( { model
                | enabledDimensions = model.enabledDimensions |> Reorderable.moveUp idx
              }
            , Cmd.none
            )

        MoveDimension idx MoveDirectionDown ->
            ( { model
                | enabledDimensions = model.enabledDimensions |> Reorderable.moveDown idx
              }
            , Cmd.none
            )

        ToggleDimension idx active ->
            let
                dimensions : ( Reorderable PartitionerDimension, List PartitionerDimension )
                dimensions =
                    ( model.enabledDimensions, model.disabledDimensions )

                ( enabled, disabled ) =
                    toggleDimension active idx dimensions
            in
            ( { model
                | enabledDimensions = enabled
                , disabledDimensions = disabled
              }
            , Cmd.none
            )

        SetGlobalFilteredAccidents _ _ ->
            -- Handled in Main module.
            ( model, Cmd.none )


toggleDimension : Bool -> Int -> ( Reorderable PartitionerDimension, List PartitionerDimension ) -> ( Reorderable PartitionerDimension, List PartitionerDimension )
toggleDimension active =
    if active then
        enableDimension

    else
        disableDimension


enableDimension : Int -> ( Reorderable PartitionerDimension, List PartitionerDimension ) -> ( Reorderable PartitionerDimension, List PartitionerDimension )
enableDimension idx ( enabled, disabled ) =
    case List.Extra.getAt idx disabled of
        Nothing ->
            ( enabled, disabled )

        Just dimension ->
            ( Reorderable.push dimension enabled, List.Extra.removeAt idx disabled )


disableDimension : Int -> ( Reorderable PartitionerDimension, List PartitionerDimension ) -> ( Reorderable PartitionerDimension, List PartitionerDimension )
disableDimension idx ( enabled, disabled ) =
    case Reorderable.get idx enabled of
        Nothing ->
            ( enabled, disabled )

        Just dimension ->
            ( Reorderable.drop idx enabled, dimension :: disabled )


{-| Represent edges as straight lines.
-}
drawLine : ( Float, Float ) -> Svg Msg
drawLine ( targetX, targetY ) =
    TypedSvg.line
        [ TypedSvg.Attributes.x1 (Px 0)
        , TypedSvg.Attributes.y1 (Px 0)
        , TypedSvg.Attributes.x2 (Px targetX)
        , TypedSvg.Attributes.y2 (Px targetY)
        , TypedSvg.Attributes.stroke (Paint Color.black)
        ]
        []


{-| Represent nodes as circles with the node value inside.
-}
drawNode : ( List Accident, String ) -> Svg Msg
drawNode ( accidents, name ) =
    TypedSvg.g
        []
        [ TypedSvg.circle
            [ TypedSvg.Attributes.r (Px 16)
            , TypedSvg.Attributes.stroke (Paint Color.black)
            , TypedSvg.Attributes.fill (Paint Color.white)
            , TypedSvg.Attributes.cx (Px 0)
            , TypedSvg.Attributes.cy (Px 0)
            ]
            []
        , TypedSvg.text_
            [ TypedSvg.Attributes.textAnchor AnchorMiddle
            , TypedSvg.Attributes.transform [ Translate 0 5, Rotate 90 0 0 ]
            , TypedSvg.Events.onClick (SetGlobalFilteredAccidents accidents name)
            ]
            [ TypedSvg.Core.text (name ++ ": " ++ String.fromInt (List.length accidents)) ]
        ]


treeGraph : Tree ( List Accident, String ) -> Html Msg
treeGraph tree =
    let
        treeDiagram : TreeDiagram.Tree ( List Accident, String )
        treeDiagram =
            toTreeDiagram tree
    in
    TreeDiagram.Svg.draw TreeDiagram.defaultTreeLayout drawNode drawLine treeDiagram
        |> fromUnstyled


type TreemapSplitAxis
    = SplitX
    | SplitY


treemap : Tree ( List Accident, String ) -> Html Msg
treemap tree =
    let
        width : Float
        width =
            500

        height : Float
        height =
            500

        padding : Float
        padding =
            20
    in
    fromUnstyled
        (svg
            [ TypedSvg.Attributes.viewBox 0 0 (width + 2 * padding) (height + 2 * padding)
            , TypedSvg.Attributes.width (Percent 50)
            , TypedSvg.Attributes.height (Percent 50)
            , TypedSvg.Attributes.preserveAspectRatio (Align ScaleMin ScaleMin) Slice
            ]
            [ TypedSvg.style [] [ TypedSvg.Core.text """
                          .node > text { display: none; }
                          .node:hover > text { display: inline; }
                        """ ]
            , TypedSvg.g
                [ TypedSvg.Attributes.transform [ Translate padding padding ] ]
                [ treemapNode SplitX width height tree ]
            ]
        )


treemapNode : TreemapSplitAxis -> Float -> Float -> Tree ( List Accident, String ) -> Svg Msg
treemapNode axis w h node =
    let
        parentLabel : ( List Accident, String )
        parentLabel =
            Tree.label node

        parentAccidents : List Accident
        parentAccidents =
            Tuple.first parentLabel

        parentWeight : Int
        parentWeight =
            List.length parentAccidents

        parentName : String
        parentName =
            Tuple.second parentLabel

        mapChildTree : Tree ( List Accident, String ) -> Tree ( List Accident, String )
        mapChildTree tree =
            Tree.mapLabel (\( num, name ) -> ( num, parentName ++ ", " ++ name )) tree

        childTrees : List (Tree ( List Accident, String ))
        childTrees =
            node
                |> Tree.mapChildren (List.map mapChildTree)
                |> Tree.children

        expandedChildTrees : List ( Float, Tree ( List Accident, String ) )
        expandedChildTrees =
            List.map
                (\tree ->
                    let
                        weight : Int
                        weight =
                            List.length (Tuple.first (Tree.label tree))

                        relativeWeight : Float
                        relativeWeight =
                            toFloat weight / toFloat parentWeight
                    in
                    ( relativeWeight, tree )
                )
                childTrees

        offsetChildTrees : ( Float, List ( Float, Float, Tree ( List Accident, String ) ) )
        offsetChildTrees =
            List.foldl
                (\( weight, tree ) ( offset, trees ) ->
                    ( offset + weight
                    , trees ++ [ ( offset, weight, tree ) ]
                    )
                )
                ( 0, [] )
                expandedChildTrees

        groups : List (Svg Msg)
        groups =
            List.map
                (\( offset, weight, tree ) ->
                    case axis of
                        SplitX ->
                            TypedSvg.g
                                [ TypedSvg.Attributes.transform [ Translate (w * offset) 0 ] ]
                                [ treemapNode SplitY (w * weight) h tree ]

                        SplitY ->
                            TypedSvg.g
                                [ TypedSvg.Attributes.transform [ Translate 0 (h * offset) ] ]
                                [ treemapNode SplitX w (h * weight) tree ]
                )
                (Tuple.second offsetChildTrees)

        nodeLabel : List (Svg Msg)
        nodeLabel =
            case groups of
                [] ->
                    [ TypedSvg.text_
                        [ TypedSvg.Attributes.textAnchor AnchorMiddle
                        , TypedSvg.Attributes.fontSize (Px 8)
                        , TypedSvg.Attributes.transform
                            [ Translate (w * 0.5) (h * 0.5)
                            ]
                        , TypedSvg.Events.onClick (SetGlobalFilteredAccidents parentAccidents parentName)
                        ]
                        [ TypedSvg.Core.text (parentName ++ ": " ++ String.fromInt parentWeight) ]
                    ]

                _ ->
                    []
    in
    TypedSvg.g
        [ TypedSvg.Attributes.class [ "node" ]
        ]
        (TypedSvg.rect
            [ TypedSvg.Attributes.x (Px 0)
            , TypedSvg.Attributes.y (Px 0)
            , TypedSvg.Attributes.width (Px w)
            , TypedSvg.Attributes.height (Px h)
            , TypedSvg.Attributes.stroke (Paint black)
            , TypedSvg.Attributes.fill (Paint black)
            , TypedSvg.Attributes.fillOpacity (Opacity 0)
            ]
            []
            :: groups
            ++ nodeLabel
        )


treeList : Tree ( List Accident, String ) -> Html Msg
treeList tree =
    let
        convertLabel : ( List Accident, String ) -> Html Msg
        convertLabel ( accidents, name ) =
            span
                [ onClick (SetGlobalFilteredAccidents accidents name) ]
                [ text (name ++ ": " ++ String.fromInt (List.length accidents)) ]

        convertTree : Html Msg -> List (Html Msg) -> Html Msg
        convertTree nodeLabel children =
            case children of
                [] ->
                    li [] [ nodeLabel ]

                _ ->
                    li [] [ nodeLabel, ul [] children ]
    in
    ul [] [ Tree.restructure convertLabel convertTree tree ]


viewTree : TreeLayout -> Tree ( List Accident, String ) -> Html Msg
viewTree layout =
    case layout of
        TreeLayoutGraph ->
            treeGraph

        TreeLayoutTreemap ->
            treemap

        TreeLayoutList ->
            treeList


buildTree : Model -> List Accident -> Tree ( List Accident, String )
buildTree model accidents =
    let
        accidentTree : Tree ( List Accident, List String )
        accidentTree =
            partitionTree (partitioners model) True accidents

        treeSize : Tree ( List a, b ) -> Int
        treeSize tree =
            tree |> Tree.label |> Tuple.first |> List.length

        sortChildrenBySize : List (Tree ( List a, b )) -> List (Tree ( List a, b ))
        sortChildrenBySize trees =
            trees |> List.sortBy treeSize

        reverseChildren : List (Tree a) -> List (Tree a)
        reverseChildren trees =
            trees |> List.reverse

        selectLabel : String -> ( a, List String ) -> ( a, String )
        selectLabel default ( num, labels ) =
            ( num, labels |> List.Extra.last |> Maybe.withDefault default )

        sortedTree : Tree ( List Accident, String )
        sortedTree =
            accidentTree |> Tree.mapChildren (sortChildrenBySize >> reverseChildren) |> Tree.map (selectLabel "all")
    in
    sortedTree


treeLayoutLabel : TreeLayout -> String
treeLayoutLabel treeLayout =
    case treeLayout of
        TreeLayoutGraph ->
            "graph"

        TreeLayoutTreemap ->
            "treemap"

        TreeLayoutList ->
            "list"


treeLayoutSelectorOption : Model -> TreeLayout -> Html Msg
treeLayoutSelectorOption model treeLayout =
    option
        [ onClick (SelectTreeLayout treeLayout)
        , selected (model.treeLayout == treeLayout)
        ]
        [ text (treeLayoutLabel treeLayout) ]


treeLayoutSelector : Model -> Html Msg
treeLayoutSelector model =
    let
        viewId : String
        viewId =
            "tree-layout-selector"

        option : TreeLayout -> Html Msg
        option =
            treeLayoutSelectorOption model

        options : List (Html Msg)
        options =
            List.map
                option
                [ TreeLayoutGraph
                , TreeLayoutTreemap
                , TreeLayoutList
                ]
    in
    form
        []
        [ Html.Styled.label
            [ Html.Styled.Attributes.for viewId ]
            [ text "Tree layout: " ]
        , select
            [ Html.Styled.Attributes.name viewId, Html.Styled.Attributes.id viewId ]
            options
        ]


dimensionLabel : Dimension -> String
dimensionLabel dimension =
    case dimension of
        DimensionLight ->
            "light condition"

        DimensionIntersection ->
            "intersection type"

        DimensionAtmosphericConditions ->
            "weather"

        DimensionCollision ->
            "collision type"

        DimensionLocationRegime ->
            "location type"

        DimensionRoadCategory ->
            "road category"

        DimensionTrafficRegime ->
            "traffic regime"

        DimensionDedicatedLane ->
            "dedicated line"

        DimensionProfile ->
            "road profile"

        DimensionCurvature ->
            "road curvature"


moveLabel : MoveDirection -> String
moveLabel direction =
    case direction of
        MoveDirectionUp ->
            "↑"

        MoveDirectionDown ->
            "↓"


canMove : Int -> Int -> MoveDirection -> Bool
canMove length index direction =
    case direction of
        MoveDirectionUp ->
            index > 0

        MoveDirectionDown ->
            index < length - 1


moveDimensionButton : Int -> Int -> MoveDirection -> Maybe (Html Msg)
moveDimensionButton length index direction =
    if canMove length index direction then
        Just
            (button
                [ onClick (MoveDimension index direction) ]
                [ text (moveLabel direction) ]
            )

    else
        Nothing


toggleDimensionButton : Int -> Bool -> Html Msg
toggleDimensionButton index active =
    input
        [ type_ "checkbox"
        , checked active
        , onClick (ToggleDimension index (not active))
        ]
        []


enabledDimensionSelectorItem : Model -> Int -> PartitionerDimension -> Html Msg
enabledDimensionSelectorItem model index ( dimension, _ ) =
    let
        length : Int
        length =
            Reorderable.length model.enabledDimensions
    in
    li
        []
        (List.filterMap identity
            [ Just (text (dimensionLabel dimension ++ " "))
            , Just (toggleDimensionButton index True)
            , moveDimensionButton length index MoveDirectionUp
            , moveDimensionButton length index MoveDirectionDown
            ]
        )


disabledDimensionSelectorItem : Int -> PartitionerDimension -> Html Msg
disabledDimensionSelectorItem index ( dimension, _ ) =
    li
        []
        [ text (dimensionLabel dimension ++ " ")
        , toggleDimensionButton index False
        ]


dimensionsSelector : Model -> Html Msg
dimensionsSelector model =
    div []
        [ ol
            []
            (model.enabledDimensions
                |> Reorderable.indexedMap (enabledDimensionSelectorItem model)
                |> Reorderable.toList
            )
        , ul
            []
            (model.disabledDimensions
                |> List.indexedMap disabledDimensionSelectorItem
            )
        ]


view : Model -> List Accident -> Html Msg
view model accidents =
    div
        []
        [ treeLayoutSelector model
        , dimensionsSelector model
        , viewTree model.treeLayout (buildTree model accidents)
        ]
