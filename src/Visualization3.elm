module Visualization3 exposing (..)

import Html.Styled exposing (Html, br, div, text)
import Model exposing (Accident)
import Reorderable exposing (Reorderable, moveDown, moveUp)


type TreeLayout
    = TreeLayoutGraph
    | TreeLayoutTreeMap


type alias Partitioner =
    Accident -> Bool


type alias Partitioners =
    List Partitioners


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
    ( Dimension, Partitioners )


type alias Model =
    { treeLayout : TreeLayout
    , dimensions : Reorderable Dimension
    }


type Msg
    = NoOp
    | SelectTreeLayout TreeLayout
    | MoveUpDimension Int
    | MoveDownDimension Int


label : String
label =
    "Accident Type Tree"


equalityPartitioner : (Accident -> a) -> a -> Partitioner
equalityPartitioner mapper value accident =
    mapper accident == value


equalityPartitioners : (Accident -> a) -> List a -> Partitioners
equalityPartitioners mapper values =
    List.map (equalityPartitioner mapper) values


init : ( Model, Cmd Msg )
init =
    ( { treeLayout = TreeLayoutTreeMap
      , dimensions =
            Reorderable.fromList
                [ DimensionCollisionType
                , DimensionRoadCategory
                , DimensionLightCondition
                , DimensionWeather
                , DimensionIntersectionType
                , DimensionRoadCurvature
                , DimensionVehicleType
                , DimensionSituation
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
