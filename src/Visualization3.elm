module Visualization3 exposing (..)

import Color exposing (black)
import Html.Styled exposing (Html, br, div, fromUnstyled, text)
import Model exposing (Accident, Collision(..), Light(..), RoadCategory(..))
import Partition exposing (Partitioner, Partitioners, equalityPartitioner)
import Reorderable exposing (Reorderable, moveDown, moveUp)
import Tree exposing (Tree)
import TypedSvg exposing (svg)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Align(..), Length(..), MeetOrSlice(..), Paint(..), Scale(..), Transform(..))


type TreeLayout
    = TreeLayoutGraph
    | TreeLayoutTreeMap


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
    ( { treeLayout = TreeLayoutTreeMap
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


type SplitAxis
    = SplitX
    | SplitY


viewTreemap : Tree Int -> Html Msg
viewTreemap tree =
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
                [ drawTreemapNode SplitX w h tree ]
            ]
        )


drawTreemapNode : SplitAxis -> Float -> Float -> Tree Int -> Svg Msg
drawTreemapNode axis w h node =
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
                                [ drawTreemapNode SplitY (w * weight) h tree ]

                        SplitY ->
                            TypedSvg.g
                                [ TypedSvg.Attributes.transform [ Translate 0 (h * offset) ] ]
                                [ drawTreemapNode SplitX w (h * weight) tree ]
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


view : Model -> List Accident -> Html Msg
view model accidents =
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        ]
