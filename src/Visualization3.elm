module Visualization3 exposing (..)

import Color exposing (black)
import Html.Styled exposing (Html, br, button, div, form, fromUnstyled, input, li, ol, option, select, text, ul)
import Html.Styled.Attributes exposing (checked, selected, type_)
import Html.Styled.Events exposing (onClick)
import Model exposing (Accident, AtmosphericConditions(..), Collision(..), Curvature(..), DedicatedLane(..), Intersection(..), Light(..), LocationRegime(..), Profile(..), RoadCategory(..), TrafficRegime(..))
import Partition exposing (Partitioner, Partitioners, equalityPartitioner, maybeEqualityPartitioner, partitionTree)
import Reorderable exposing (Reorderable)
import Tree exposing (Tree)
import TypedSvg exposing (svg)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Align(..), Length(..), MeetOrSlice(..), Paint(..), Scale(..), Transform(..))


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
    ( Dimension, Partitioner Accident, Bool )


type alias Model =
    { treeLayout : TreeLayout
    , dimensions : Reorderable PartitionerDimension
    }


type MoveDirection
    = MoveDirectionUp
    | MoveDirectionDown


type Msg
    = NoOp
    | SelectTreeLayout TreeLayout
    | MoveDimension Int MoveDirection
    | ToggleDimension Int Bool


label : String
label =
    "Accident Type Tree"


initDimensions : List PartitionerDimension
initDimensions =
    [ ( DimensionLight
      , maybeEqualityPartitioner .light
            [ LightDaylight
            , LightDuskOrDawn
            , LightNightWithoutPublicLighting
            , LightNightWithPublicLightingOff
            , LightNightWithPublicLightingOn
            ]
      , True
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
      , True
      )
    , ( DimensionAtmosphericConditions
      , maybeEqualityPartitioner .atmospheric_conditions
            [ AtmosphericConditionsNormal
            , AtmosphericConditionsLightRain
            , AtmosphericConditionsHeavyRain
            , AtmosphericConditionsSnowHail
            , AtmosphericConditionsFogSmoke
            , AtmosphericConditionsStrongWindStorm
            , AtmosphericConditionsDazzlingWeather
            , AtmosphericConditionsOvercastWeather
            ]
      , False
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
      , False
      )
    , ( DimensionLocationRegime
      , maybeEqualityPartitioner .location
            [ LocationRegimeOutOfTown
            , LocationRegimeInBuiltUpAreas
            ]
      , False
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
      , True
      )
    , ( DimensionTrafficRegime
      , maybeEqualityPartitioner .traffic_regime
            [ TrafficRegimeOneWay
            , TrafficRegimeBidirectional
            , TrafficRegimeWithSeparateLanes
            , TrafficRegimeWithVariableAssignmentLanes
            ]
      , False
      )
    , ( DimensionDedicatedLane
      , maybeEqualityPartitioner .dedicated_lane
            [ DedicatedLaneNone
            , DedicatedLaneBicyclePath
            , DedicatedLaneCycleLane
            , DedicatedLaneReservedLane
            ]
      , False
      )
    , ( DimensionProfile
      , maybeEqualityPartitioner .profile
            [ ProfileFlat
            , ProfileSlope
            , ProfileTopOfHill
            , ProfileBottomOfHill
            ]
      , True
      )
    , ( DimensionCurvature
      , maybeEqualityPartitioner .curvature
            [ CurvatureStraight
            , CurvatureLeftHandCurve
            , CurvatureRightHandCurve
            , CurvatureSCurve
            ]
      , True
      )
    ]


init : ( Model, Cmd Msg )
init =
    ( { treeLayout = TreeLayoutTreemap
      , dimensions = Reorderable.fromList initDimensions
      }
    , Cmd.none
    )


partitioners : Model -> Partitioners Accident
partitioners model =
    model.dimensions
        |> Reorderable.toList
        |> List.filter (\( _, _, active ) -> active)
        |> List.map (\( _, partitioner, _ ) -> partitioner)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectTreeLayout layout ->
            ( { model | treeLayout = layout }, Cmd.none )

        MoveDimension idx MoveDirectionUp ->
            ( { model
                | dimensions = Reorderable.moveUp idx model.dimensions
              }
            , Cmd.none
            )

        MoveDimension idx MoveDirectionDown ->
            ( { model
                | dimensions = Reorderable.moveDown idx model.dimensions
              }
            , Cmd.none
            )

        ToggleDimension idx active ->
            ( { model
                | dimensions =
                    Reorderable.update
                        idx
                        (\( dimension, partitioner, _ ) -> ( dimension, partitioner, active ))
                        model.dimensions
              }
            , Cmd.none
            )


type TreemapSplitAxis
    = SplitX
    | SplitY


treemap : Tree Int -> Html Msg
treemap tree =
    let
        w =
            500

        h =
            500

        padding =
            20
    in
    fromUnstyled
        (svg
            [ TypedSvg.Attributes.viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
            , TypedSvg.Attributes.width (Percent 50)
            , TypedSvg.Attributes.height (Percent 50)
            , TypedSvg.Attributes.preserveAspectRatio (Align ScaleMin ScaleMin) Slice
            ]
            [ TypedSvg.g
                [ TypedSvg.Attributes.transform [ Translate padding padding ] ]
                [ treemapNode SplitX w h tree ]
            ]
        )


treemapNode : TreemapSplitAxis -> Float -> Float -> Tree Int -> Svg Msg
treemapNode axis w h node =
    let
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
                        weight =
                            Tree.label tree

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
viewTree layout tree =
    case layout of
        TreeLayoutGraph ->
            -- todo
            text "Not yet implemented."

        TreeLayoutTreemap ->
            treemap tree

        TreeLayoutList ->
            treeList tree


buildTree : Model -> List Accident -> Tree Int
buildTree model accidents =
    let
        accidentTree =
            partitionTree accidents (partitioners model)
    in
    Tree.map List.length accidentTree


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
        viewId =
            "tree-layout-selector"

        option =
            treeLayoutSelectorOption model

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


canMove : Reorderable a -> Int -> MoveDirection -> Bool
canMove items index direction =
    case direction of
        MoveDirectionUp ->
            index > 0

        MoveDirectionDown ->
            index < Reorderable.length items - 1


moveDimensionButton : Reorderable a -> Int -> MoveDirection -> Maybe (Html Msg)
moveDimensionButton dimensions index direction =
    if canMove dimensions index direction then
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


dimensionsSelector : Model -> Html Msg
dimensionsSelector model =
    ol
        []
        (List.indexedMap
            (\index ( dimension, _, active ) ->
                li
                    []
                    (List.filterMap identity
                        [ Just (text (dimensionLabel dimension ++ " "))
                        , Just (toggleDimensionButton index active)
                        , moveDimensionButton model.dimensions index MoveDirectionUp
                        , moveDimensionButton model.dimensions index MoveDirectionDown
                        ]
                    )
            )
            (Reorderable.toList model.dimensions)
        )


view : Model -> List Accident -> Html Msg
view model accidents =
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        , treeLayoutSelector model
        , dimensionsSelector model
        , viewTree model.treeLayout (buildTree model accidents)
        ]
