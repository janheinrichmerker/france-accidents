module Visualization3 exposing (..)

import Html.Styled exposing (Html, br, div, text)
import Model exposing (Accident, Collision(..), Light(..), RoadCategory(..))
import Partition exposing (Partitioner, Partitioners, equalityPartitioner)
import Reorderable exposing (Reorderable, moveDown, moveUp)


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


view : Model -> List Accident -> Html Msg
view model accidents =
    div
        []
        [ text label
        , br [] []
        , text (String.fromInt (List.length accidents))
        ]
