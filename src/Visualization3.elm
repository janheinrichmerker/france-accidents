module Visualization3 exposing (..)

import Color exposing (black)
import Html.Styled exposing (Html, br, div, form, fromUnstyled, li, option, select, text, ul)
import Html.Styled.Attributes exposing (selected)
import Html.Styled.Events exposing (onClick)
import Model exposing (Accident, Collision(..), Light(..), RoadCategory(..))
import Partition exposing (Partitioner, Partitioners, equalityPartitioner, partitionTree)
import Reorderable exposing (Reorderable, moveDown, moveUp)
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
    = DimensionCollisionType
    | DimensionRoadCategory
    | DimensionLightCondition
    | DimensionWeather
    | DimensionIntersectionType
    | DimensionRoadCurvature
    | DimensionVehicleType
    | DimensionSituation


type alias PartitionersDimension =
    ( Dimension, Partitioner Accident )


type alias Model =
    { treeLayout : TreeLayout
    , dimensions : Reorderable PartitionersDimension
    }


type Msg
    = NoOp
    | SelectTreeLayout TreeLayout
    | MoveUpDimension Int
    | MoveDownDimension Int


label : String
label =
    "Accident Type Tree"


init : ( Model, Cmd Msg )
init =
    ( { treeLayout = TreeLayoutTreemap
      , dimensions =
            Reorderable.fromList
                [ ( DimensionCollisionType
                  , equalityPartitioner .collision
                        [ Just CollisionTwoVehiclesFront
                        , Just CollisionTwoVehiclesFromTheRear
                        , Just CollisionTwoVehiclesFromTheSide
                        , Just CollisionThreeOrMoreVehiclesInAChain
                        , Just CollisionThreeOrMoreVehiclesMultipleCollisions
                        , Just CollisionOtherCollision
                        , Just CollisionWithoutCollision
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
                        , RoadCategoryOther
                        ]
                  )
                , ( DimensionLightCondition
                  , equalityPartitioner .light
                        [ Just LightDaylight
                        , Just LightDuskOrDawn
                        , Just LightNightWithoutPublicLighting
                        , Just LightNightWithPublicLightingOff
                        , Just LightNightWithPublicLightingOn
                        ]
                  )
                , ( DimensionWeather
                  , []
                    -- todo
                  )
                , ( DimensionIntersectionType
                  , []
                    -- todo
                  )
                , ( DimensionRoadCurvature
                  , []
                    -- todo
                  )
                , ( DimensionVehicleType
                  , []
                    -- todo
                  )
                , ( DimensionSituation
                  , []
                    -- todo
                  )
                ]
      }
    , Cmd.none
    )


partitioners : Model -> Partitioners Accident
partitioners model =
    List.map Tuple.second (Reorderable.toList model.dimensions)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectTreeLayout layout ->
            ( { model | treeLayout = layout }, Cmd.none )

        MoveUpDimension idx ->
            ( { model | dimensions = moveUp idx model.dimensions }, Cmd.none )

        MoveDownDimension idx ->
            ( { model | dimensions = moveDown idx model.dimensions }, Cmd.none )


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


view : Model -> List Accident -> Html Msg
view model accidents =
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        , treeLayoutSelector model
        , viewTree model.treeLayout (buildTree model accidents)
        ]
