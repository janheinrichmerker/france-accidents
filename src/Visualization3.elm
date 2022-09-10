module Visualization3 exposing (..)

import Color exposing (black)
import Html.Styled exposing (Html, button, div, form, fromUnstyled, input, li, ol, option, select, text, ul)
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
import TypedSvg.Types exposing (Align(..), AnchorAlignment(..), Length(..), MeetOrSlice(..), Paint(..), Scale(..), Transform(..))


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
    ( Dimension, Partitioner Accident )


type alias Model =
    { treeLayout : TreeLayout
    , enabledDimensions : Reorderable PartitionerDimension
    , disabledDimensions : List PartitionerDimension
    }


type MoveDirection
    = MoveDirectionUp
    | MoveDirectionDown


type Msg
    = SelectTreeLayout TreeLayout
    | MoveDimension Int MoveDirection
    | ToggleDimension Int Bool


label : String
label =
    "Accident Type Tree"


initEnabledDimensions : List PartitionerDimension
initEnabledDimensions =
    [ ( DimensionLight
      , maybeEqualityPartitioner .light
            [ LightDaylight
            , LightDuskOrDawn
            , LightNightWithoutPublicLighting
            , LightNightWithPublicLightingOff
            , LightNightWithPublicLightingOn
            ]
      )
    , ( DimensionIntersection
      , maybeEqualityPartitioner .intersection
            [ IntersectionOutOfIntersection
            , IntersectionXIntersection
            , IntersectionTIntersection
            , IntersectionYIntersection
            , IntersectionIntersectionWithMoreThan4Branches
            , IntersectionRoundabout
            , IntersectionPlace
            , IntersectionLevelCrossing
            , IntersectionOtherIntersection
            ]
      )
    , ( DimensionRoadCategory
      , equalityPartitioner .road_category
            [ RoadCategoryHighway
            , RoadCategoryNationalRoad
            , RoadCategoryDepartmentalRoad
            , RoadCategoryMunicipalRoads
            , RoadCategoryOffThePublicNetwork
            , RoadCategoryParkingLotOpenToPublicTraffic
            , RoadCategoryUrbanMetropolitanRoads
            ]
      )
    , ( DimensionProfile
      , maybeEqualityPartitioner .profile
            [ ProfileFlat
            , ProfileSlope
            , ProfileTopOfHill
            , ProfileBottomOfHill
            ]
      )
    , ( DimensionCurvature
      , maybeEqualityPartitioner .curvature
            [ CurvatureStraight
            , CurvatureLeftHandCurve
            , CurvatureRightHandCurve
            , CurvatureSCurve
            ]
      )
    ]


initDisabledDimensions : List PartitionerDimension
initDisabledDimensions =
    [ ( DimensionAtmosphericConditions
      , maybeEqualityPartitioner .atmosphericConditions
            [ AtmosphericConditionsNormal
            , AtmosphericConditionsLightRain
            , AtmosphericConditionsHeavyRain
            , AtmosphericConditionsSnowHail
            , AtmosphericConditionsFogSmoke
            , AtmosphericConditionsStrongWindStorm
            , AtmosphericConditionsDazzlingWeather
            , AtmosphericConditionsOvercastWeather
            ]
      )
    , ( DimensionCollision
      , maybeEqualityPartitioner .collision
            [ CollisionTwoVehiclesFront
            , CollisionTwoVehiclesFromTheRear
            , CollisionTwoVehiclesFromTheSide
            , CollisionThreeOrMoreVehiclesInAChain
            , CollisionThreeOrMoreVehiclesMultipleCollisions
            , CollisionWithoutCollision
            ]
      )
    , ( DimensionLocationRegime
      , maybeEqualityPartitioner .location
            [ LocationRegimeOutOfTown
            , LocationRegimeInBuiltUpAreas
            ]
      )
    , ( DimensionTrafficRegime
      , maybeEqualityPartitioner .traffic_regime
            [ TrafficRegimeOneWay
            , TrafficRegimeBidirectional
            , TrafficRegimeWithSeparateLanes
            , TrafficRegimeWithVariableAssignmentLanes
            ]
      )
    , ( DimensionDedicatedLane
      , maybeEqualityPartitioner .dedicated_lane
            [ DedicatedLaneNone
            , DedicatedLaneBicyclePath
            , DedicatedLaneCycleLane
            , DedicatedLaneReservedLane
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


partitioners : Model -> Partitioners Accident
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
drawNode : Int -> Svg Msg
drawNode name =
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
            ]
            [ TypedSvg.Core.text (String.fromInt name) ]
        ]


treeGraph : Tree Int -> Html Msg
treeGraph tree =
    let
        treeDiagram : TreeDiagram.Tree Int
        treeDiagram =
            toTreeDiagram tree
    in
    TreeDiagram.Svg.draw TreeDiagram.defaultTreeLayout drawNode drawLine treeDiagram
        |> fromUnstyled


type TreemapSplitAxis
    = SplitX
    | SplitY


treemap : Tree Int -> Html Msg
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
            [ TypedSvg.g
                [ TypedSvg.Attributes.transform [ Translate padding padding ] ]
                [ treemapNode SplitX width height tree ]
            ]
        )


treemapNode : TreemapSplitAxis -> Float -> Float -> Tree Int -> Svg Msg
treemapNode axis w h node =
    let
        parentWeight : Int
        parentWeight =
            Tree.label node

        childTrees : List (Tree Int)
        childTrees =
            Tree.children node

        expandedChildTrees : List ( Float, Tree Int )
        expandedChildTrees =
            List.map
                (\tree ->
                    let
                        weight : Int
                        weight =
                            Tree.label tree

                        relativeWeight : Float
                        relativeWeight =
                            toFloat weight / toFloat parentWeight
                    in
                    ( relativeWeight, tree )
                )
                childTrees

        offsetChildTrees : ( Float, List ( Float, Float, Tree Int ) )
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
    in
    TypedSvg.g
        []
        (TypedSvg.rect
            [ TypedSvg.Attributes.x (Px 0)
            , TypedSvg.Attributes.y (Px 0)
            , TypedSvg.Attributes.width (Px w)
            , TypedSvg.Attributes.height (Px h)
            , TypedSvg.Attributes.stroke (Paint black)
            , TypedSvg.Attributes.fill PaintNone
            ]
            []
            :: groups
        )


treeList : Tree Int -> Html Msg
treeList tree =
    let
        convertLabel : Int -> Html Msg
        convertLabel nodeLabel =
            text (String.fromInt nodeLabel)

        convertTree : Html Msg -> List (Html Msg) -> Html Msg
        convertTree nodeLabel children =
            case children of
                [] ->
                    li [] [ nodeLabel ]

                _ ->
                    li []
                        [ nodeLabel
                        , ul [] children
                        ]
    in
    ul [] [ Tree.restructure convertLabel convertTree tree ]


viewTree : TreeLayout -> Tree Int -> Html Msg
viewTree layout =
    case layout of
        TreeLayoutGraph ->
            treeGraph

        TreeLayoutTreemap ->
            treemap

        TreeLayoutList ->
            treeList


buildTree : Model -> List Accident -> Tree Int
buildTree model accidents =
    let
        accidentTree : Tree (List Accident)
        accidentTree =
            partitionTree accidents (partitioners model)

        treeSize : Tree (List a) -> Int
        treeSize tree =
            tree |> Tree.label |> List.length

        sortChildrenBySize : List (Tree (List a)) -> List (Tree (List a))
        sortChildrenBySize trees =
            trees |> List.sortBy treeSize

        reverseChildren : List (Tree (List a)) -> List (Tree (List a))
        reverseChildren trees =
            trees |> List.reverse

        sortedTree : Tree (List Accident)
        sortedTree =
            accidentTree |> Tree.mapChildren (sortChildrenBySize >> reverseChildren)
    in
    Tree.map List.length sortedTree


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
