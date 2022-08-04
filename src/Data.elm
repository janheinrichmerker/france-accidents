module Data exposing (..)

import Http
import Iso8601
import Json.Decode exposing (Decoder, andThen, decodeString, fail, float, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Model exposing (Accident, AtmosphericConditions(..), Collision(..), Curvature(..), DedicatedLane(..), Engine(..), FixedObstacle(..), Intersection(..), Light(..), LocationRegime(..), Manoeuvre(..), MobileObstacle(..), PedestrianAction(..), PedestrianCompany(..), PedestrianLocation(..), Person, PersonCategory(..), Place(..), Profile(..), RoadCategory(..), SafetyEquipment(..), Severity(..), Sex(..), ShockPoint(..), TrafficDirection(..), TrafficRegime(..), TravelReason(..), Vehicle, VehicleCategory(..))
import Result as Http


light : Decoder Light
light =
    string
        |> andThen
            (\str ->
                case str of
                    "DAYLIGHT" ->
                        succeed LightDaylight

                    "DUSK_OR_DAWN" ->
                        succeed LightDuskOrDawn

                    "NIGHT_WITHOUT_PUBLIC_LIGHTING" ->
                        succeed LightNightWithoutPublicLighting

                    "NIGHT_WITH_PUBLIC_LIGHTING_OFF" ->
                        succeed LightNightWithPublicLightingOff

                    "NIGHT_WITH_PUBLIC_LIGHTING_ON" ->
                        succeed LightNightWithPublicLightingOn

                    unknown ->
                        fail ("Unknown light: " ++ unknown)
            )


intersection : Decoder Intersection
intersection =
    string
        |> andThen
            (\str ->
                case str of
                    "OUT_OF_INTERSECTION" ->
                        succeed IntersectionOutOfIntersection

                    "XINTERSECTION" ->
                        succeed IntersectionXIntersection

                    "TINTERSECTION" ->
                        succeed IntersectionTIntersection

                    "YINTERSECTION" ->
                        succeed IntersectionYIntersection

                    "INTERSECTION_WITH_MORE_THAN_4_BRANCHES" ->
                        succeed IntersectionIntersectionWithMoreThan4Branches

                    "ROUNDABOUT" ->
                        succeed IntersectionRoundabout

                    "PLACE" ->
                        succeed IntersectionPlace

                    "LEVEL_CROSSING" ->
                        succeed IntersectionLevelCrossing

                    "OTHER_INTERSECTION" ->
                        succeed IntersectionOtherIntersection

                    unknown ->
                        fail ("Unknown intersection: " ++ unknown)
            )


atmosphericConditions : Decoder AtmosphericConditions
atmosphericConditions =
    string
        |> andThen
            (\str ->
                case str of
                    "NORMAL" ->
                        succeed AtmosphericConditionsNormal

                    "LIGHT_RAIN" ->
                        succeed AtmosphericConditionsLightRain

                    "HEAVY_RAIN" ->
                        succeed AtmosphericConditionsHeavyRain

                    "SNOW_HAIL" ->
                        succeed AtmosphericConditionsSnowHail

                    "FOG_SMOKE" ->
                        succeed AtmosphericConditionsFogSmoke

                    "STRONG_WIND_STORM" ->
                        succeed AtmosphericConditionsStrongWindStorm

                    "DAZZLING_WEATHER" ->
                        succeed AtmosphericConditionsDazzlingWeather

                    "OVERCAST_WEATHER" ->
                        succeed AtmosphericConditionsOvercastWeather

                    "OTHER" ->
                        succeed AtmosphericConditionsOther

                    unknown ->
                        fail ("Unknown atmospheric conditions: " ++ unknown)
            )


collision : Decoder Collision
collision =
    string
        |> andThen
            (\str ->
                case str of
                    "TWO_VEHICLES_FRONT" ->
                        succeed CollisionTwoVehiclesFront

                    "TWO_VEHICLES_FROM_THE_REAR" ->
                        succeed CollisionTwoVehiclesFromTheRear

                    "TWO_VEHICLES_FROM_THE_SIDE" ->
                        succeed CollisionTwoVehiclesFromTheSide

                    "THREE_OR_MORE_VEHICLES_IN_ACHAIN" ->
                        succeed CollisionThreeOrMoreVehiclesInAChain

                    "THREE_OR_MORE_VEHICLES_MULTIPLE_COLLISIONS" ->
                        succeed CollisionThreeOrMoreVehiclesMultipleCollisions

                    "OTHER_COLLISION" ->
                        succeed CollisionOtherCollision

                    "WITHOUT_COLLISION" ->
                        succeed CollisionWithoutCollision

                    unknown ->
                        fail ("Unknown collision: " ++ unknown)
            )


locationRegime : Decoder LocationRegime
locationRegime =
    string
        |> andThen
            (\str ->
                case str of
                    "OUT_OF_TOWN" ->
                        succeed LocationRegimeOutOfTown

                    "IN_BUILT_UP_AREAS" ->
                        succeed LocationRegimeInBuiltUpAreas

                    unknown ->
                        fail ("Unknown location regime: " ++ unknown)
            )


roadCategory : Decoder RoadCategory
roadCategory =
    string
        |> andThen
            (\str ->
                case str of
                    "HIGHWAY" ->
                        succeed RoadCategoryHighway

                    "NATIONAL_ROAD" ->
                        succeed RoadCategoryNationalRoad

                    "DEPARTMENTAL_ROAD" ->
                        succeed RoadCategoryDepartmentalRoad

                    "MUNICIPAL_ROADS" ->
                        succeed RoadCategoryMunicipalRoads

                    "OFF_THE_PUBLIC_NETWORK" ->
                        succeed RoadCategoryOffThePublicNetwork

                    "PARKING_LOT_OPEN_TO_PUBLIC_TRAFFIC" ->
                        succeed RoadCategoryParkingLotOpenToPublicTraffic

                    "URBAN_METROPOLITAN_ROADS" ->
                        succeed RoadCategoryUrbanMetropolitanRoads

                    "OTHER" ->
                        succeed RoadCategoryOther

                    unknown ->
                        fail ("Unknown road category: " ++ unknown)
            )


trafficRegime : Decoder TrafficRegime
trafficRegime =
    string
        |> andThen
            (\str ->
                case str of
                    "ONE_WAY" ->
                        succeed TrafficRegimeOneWay

                    "BIDIRECTIONAL" ->
                        succeed TrafficRegimeBidirectional

                    "WITH_SEPARATE_LANES" ->
                        succeed TrafficRegimeWithSeparateLanes

                    "WITH_VARIABLE_ASSIGNMENT_LANES" ->
                        succeed TrafficRegimeWithVariableAssignmentLanes

                    unknown ->
                        fail ("Unknown traffic regime: " ++ unknown)
            )


dedicatedLane : Decoder DedicatedLane
dedicatedLane =
    string
        |> andThen
            (\str ->
                case str of
                    "NONE" ->
                        succeed DedicatedLaneNone

                    "BICYCLE_PATH" ->
                        succeed DedicatedLaneBicyclePath

                    "CYCLE_LANE" ->
                        succeed DedicatedLaneCycleLane

                    "RESERVED_LANE" ->
                        succeed DedicatedLaneReservedLane

                    unknown ->
                        fail ("Unknown dedicated lane: " ++ unknown)
            )


profile : Decoder Profile
profile =
    string
        |> andThen
            (\str ->
                case str of
                    "FLAT" ->
                        succeed ProfileFlat

                    "SLOPE" ->
                        succeed ProfileSlope

                    "TOP_OF_HILL" ->
                        succeed ProfileTopOfHill

                    "BOTTOM_OF_HILL" ->
                        succeed ProfileBottomOfHill

                    unknown ->
                        fail ("Unknown profile: " ++ unknown)
            )


curvature : Decoder Curvature
curvature =
    string
        |> andThen
            (\str ->
                case str of
                    "STRAIGHT" ->
                        succeed CurvatureStraight

                    "LEFT_HAND_CURVE" ->
                        succeed CurvatureLeftHandCurve

                    "RIGHT_HAND_CURVE" ->
                        succeed CurvatureRightHandCurve

                    "SCURVE" ->
                        succeed CurvatureSCurve

                    unknown ->
                        fail ("Unknown curvature: " ++ unknown)
            )


place : Decoder Place
place =
    string
        |> andThen
            (\str ->
                case str of
                    "FRONT_LEFT_MOTORCYCLE_FRONT" ->
                        succeed PlaceFrontLeftMotorcycleFront

                    "FRONT_RIGHT_MOTORCYCLE_REAR" ->
                        succeed PlaceFrontRightMotorcycleRear

                    "REAR_RIGHT_MOTORCYCLE_SIDECAR" ->
                        succeed PlaceRearRightMotorcycleSidecar

                    "REAR_LEFT" ->
                        succeed PlaceRearLeft

                    "REAR_CENTER" ->
                        succeed PlaceRearCenter

                    "FRONT_CENTER" ->
                        succeed PlaceFrontCenter

                    "CENTER_LEFT" ->
                        succeed PlaceCenterLeft

                    "CENTER_MIDDLE" ->
                        succeed PlaceCenterMiddle

                    "CENTER_RIGHT" ->
                        succeed PlaceCenterRight

                    "PEDESTRIAN" ->
                        succeed PlacePedestrian

                    unknown ->
                        fail ("Unknown place: " ++ unknown)
            )


personCategory : Decoder PersonCategory
personCategory =
    string
        |> andThen
            (\str ->
                case str of
                    "DRIVER" ->
                        succeed PersonCategoryDriver

                    "PASSENGER" ->
                        succeed PersonCategoryPassenger

                    "PEDESTRIAN" ->
                        succeed PersonCategoryPedestrian

                    unknown ->
                        fail ("Unknown person category: " ++ unknown)
            )


severity : Decoder Severity
severity =
    string
        |> andThen
            (\str ->
                case str of
                    "UNHARMED" ->
                        succeed SeverityUnharmed

                    "KILLED" ->
                        succeed SeverityKilled

                    "INJURED_HOSPITALIZED" ->
                        succeed SeverityInjuredHospitalized

                    "SLIGHTLY_INJURED" ->
                        succeed SeveritySlightlyInjured

                    unknown ->
                        fail ("Unknown severity: " ++ unknown)
            )


sex : Decoder Sex
sex =
    string
        |> andThen
            (\str ->
                case str of
                    "FEMALE" ->
                        succeed SexFemale

                    "MALE" ->
                        succeed SexMale

                    unknown ->
                        fail ("Unknown sex: " ++ unknown)
            )


travelReason : Decoder TravelReason
travelReason =
    string
        |> andThen
            (\str ->
                case str of
                    "HOME_TO_WORK" ->
                        succeed TravelReasonHomeToWork

                    "HOME_TO_SCHOOL" ->
                        succeed TravelReasonHomeToSchool

                    "SHOPPING" ->
                        succeed TravelReasonShopping

                    "PROFESSIONAL" ->
                        succeed TravelReasonProfessional

                    "WALKING_LEISURE" ->
                        succeed TravelReasonWalkingLeisure

                    "OTHER" ->
                        succeed TravelReasonOther

                    unknown ->
                        fail ("Unknown travel reason: " ++ unknown)
            )


safetyEquipment : Decoder SafetyEquipment
safetyEquipment =
    string
        |> andThen
            (\str ->
                case str of
                    "BELT" ->
                        succeed SafetyEquipmentBelt

                    "HEADSET" ->
                        succeed SafetyEquipmentHeadset

                    "CHILDREN_DEVICE" ->
                        succeed SafetyEquipmentChildrenDevice

                    "REFLECTIVE_VEST" ->
                        succeed SafetyEquipmentReflectiveVest

                    "AIRBAG" ->
                        succeed SafetyEquipmentAirbag

                    "GLOVES" ->
                        succeed SafetyEquipmentGloves

                    "OTHER" ->
                        succeed SafetyEquipmentOther

                    unknown ->
                        fail ("Unknown safety equipment: " ++ unknown)
            )


pedestrianLocation : Decoder PedestrianLocation
pedestrianLocation =
    string
        |> andThen
            (\str ->
                case str of
                    "ON_PAVEMENT_AT_LEAST_50_MFROM_PEDESTRIAN_CROSSING" ->
                        succeed PedestrianLocationOnPavementAtLeast50MFromPedestrianCrossing

                    "ON_PAVEMENT_AT_MOST_50_MFROM_PEDESTRIAN_CROSSING" ->
                        succeed PedestrianLocationOnPavementAtMost50MFromPedestrianCrossing

                    "ON_PEDESTRIAN_CROSSING_WITHOUT_LIGHT_SIGNAL" ->
                        succeed PedestrianLocationOnPedestrianCrossingWithoutLightSignal

                    "ON_PEDESTRIAN_CROSSING_WITH_LIGHT_SIGNAL" ->
                        succeed PedestrianLocationOnPedestrianCrossingWithLightSignal

                    "ON_SIDEWALK" ->
                        succeed PedestrianLocationOnSidewalk

                    "ON_ROAD_SHOULDER" ->
                        succeed PedestrianLocationOnRoadShoulder

                    "ON_EMERGENCY_BAY_OR_SHOULDER" ->
                        succeed PedestrianLocationOnEmergencyBayOrShoulder

                    "ON_COUNTER_AISLE" ->
                        succeed PedestrianLocationOnCounterAisle

                    unknown ->
                        fail ("Unknown pedestrian location: " ++ unknown)
            )


pedestrianAction : Decoder PedestrianAction
pedestrianAction =
    string
        |> andThen
            (\str ->
                case str of
                    "DIRECTION_OF_IMPACTING_VEHICLE" ->
                        succeed PedestrianActionDirectionOfImpactingVehicle

                    "OPPOSITE_DIRECTION_OF_IMPACTING_VEHICLE" ->
                        succeed PedestrianActionOppositeDirectionOfImpactingVehicle

                    "CROSSING" ->
                        succeed PedestrianActionCrossing

                    "MASKED" ->
                        succeed PedestrianActionMasked

                    "PLAYING_RUNNING" ->
                        succeed PedestrianActionPlayingRunning

                    "WITH_ANIMAL" ->
                        succeed PedestrianActionWithAnimal

                    "OTHER" ->
                        succeed PedestrianActionOther

                    "GETTING_ON_OFF_VEHICLE" ->
                        succeed PedestrianActionGettingOnOffVehicle

                    unknown ->
                        fail ("Unknown pedestrian action: " ++ unknown)
            )


pedestrianCompany : Decoder PedestrianCompany
pedestrianCompany =
    string
        |> andThen
            (\str ->
                case str of
                    "ALONE" ->
                        succeed PedestrianCompanyAlone

                    "ACCOMPANIED" ->
                        succeed PedestrianCompanyAccompanied

                    "GROUP" ->
                        succeed PedestrianCompanyGroup

                    unknown ->
                        fail ("Unknown pedestrian company: " ++ unknown)
            )


trafficDirection : Decoder TrafficDirection
trafficDirection =
    string
        |> andThen
            (\str ->
                case str of
                    "INCREASING_REFERENCE_NUMBER" ->
                        succeed TrafficDirectionIncreasingReferenceNumber

                    "DECREASING_REFERENCE_NUMBER" ->
                        succeed TrafficDirectionDecreasingReferenceNumber

                    "NO_REFERENCE" ->
                        succeed TrafficDirectionNoReference

                    unknown ->
                        fail ("Unknown traffic direction: " ++ unknown)
            )


vehicleCategory : Decoder VehicleCategory
vehicleCategory =
    string
        |> andThen
            (\str ->
                case str of
                    "BICYCLE" ->
                        succeed VehicleCategoryBicycle

                    "MOPED_LESS_THAN_50_CM_3" ->
                        succeed VehicleCategoryMopedLessThan50Cm3

                    "QUAD_WITH_BODY" ->
                        succeed VehicleCategoryQuadWithBody

                    "REGISTERED_SCOOTER" ->
                        succeed VehicleCategoryRegisteredScooter

                    "MOTORCYCLE" ->
                        succeed VehicleCategoryMotorcycle

                    "SIDECAR" ->
                        succeed VehicleCategorySidecar

                    "LIGHT_VEHICLE_ONLY" ->
                        succeed VehicleCategoryLightVehicleOnly

                    "LIGHT_VEHICLE_WITH_CARAVAN" ->
                        succeed VehicleCategoryLightVehicleWithCaravan

                    "LIGHT_VEHICLE_WITH_TRAILER" ->
                        succeed VehicleCategoryLightVehicleWithTrailer

                    "COMMERCIAL_VEHICLE_ONLY_WEIGHT_RATING_AT_LEAST_1_5_TAT_MOST_3_5_T" ->
                        succeed VehicleCategoryCommercialVehicleOnlyWeightRatingAtLeast1_5TAtMost3_5T

                    "COMMERCIAL_VEHICLE_WITH_CARAVAN_WEIGHT_RATING_AT_LEAST_1_5_TAT_MOST_3_5_T" ->
                        succeed VehicleCategoryCommercialVehicleWithCaravanWeightRatingAtLeast1_5TAtMost3_5T

                    "COMMERCIAL_VEHICLE_WITH_TRAILER_WEIGHT_RATING_AT_LEAST_1_5_TAT_MOST_3_5_T" ->
                        succeed VehicleCategoryCommercialVehicleWithTrailerWeightRatingAtLeast1_5TAtMost3_5T

                    "TRUCK_ONLY_WEIGHT_RATING_MORE_THAN_3_5_TAT_MOST_7_5_T" ->
                        succeed VehicleCategoryTruckOnlyWeightRatingMoreThan3_5TAtMost7_5T

                    "TRUCK_ONLY_WEIGHT_RATING_MORE_THAN_7_5_T" ->
                        succeed VehicleCategoryTruckOnlyWeightRatingMoreThan7_5T

                    "TRUCK_MORE_THAN_3_5_TWITH_TRAILER" ->
                        succeed VehicleCategoryTruckMoreThan3_5TWithTrailer

                    "ROAD_TRACTOR_ONLY" ->
                        succeed VehicleCategoryRoadTractorOnly

                    "ROAD_TRACTOR_WITH_SEMI_TRAILER" ->
                        succeed VehicleCategoryRoadTractorWithSemiTrailer

                    "PUBLIC_TRANSPORT" ->
                        succeed VehicleCategoryPublicTransport

                    "SPECIAL_MACHINE" ->
                        succeed VehicleCategorySpecialMachine

                    "AGRICULTURAL_TRACTOR" ->
                        succeed VehicleCategoryAgriculturalTractor

                    "SCOOTER_LESS_THAN_50_CM_3" ->
                        succeed VehicleCategoryScooterLessThan50Cm3

                    "MOTORCYCLE_MORE_THAN_50_CM_3_AT_MOST_125_CM_3" ->
                        succeed VehicleCategoryMotorcycleMoreThan50Cm3AtMost125Cm3

                    "SCOOTER_MORE_THAN_50_CM_3_AT_MOST_125_CM_3" ->
                        succeed VehicleCategoryScooterMoreThan50Cm3AtMost125Cm3

                    "MOTORCYCLE_MORE_THAN_125_CM_3" ->
                        succeed VehicleCategoryMotorcycleMoreThan125Cm3

                    "SCOOTER_MORE_THAN_125_CM_3" ->
                        succeed VehicleCategoryScooterMoreThan125Cm3

                    "QUAD_WITHOUT_BODY_AT_MOST_50_CM_3" ->
                        succeed VehicleCategoryQuadWithoutBodyAtMost50Cm3

                    "QUAD_WITHOUT_BODY_MORE_THAN_50_CM_3" ->
                        succeed VehicleCategoryQuadWithoutBodyMoreThan50Cm3

                    "BUSES" ->
                        succeed VehicleCategoryBuses

                    "BUS" ->
                        succeed VehicleCategoryBus

                    "TRAIN" ->
                        succeed VehicleCategoryTrain

                    "TRAMWAY" ->
                        succeed VehicleCategoryTramway

                    "TRICYCLE_AT_MOST_50_CM_3" ->
                        succeed VehicleCategoryTricycleAtMost50Cm3

                    "TRICYCLE_MORE_THAN_50_CM_3_AT_MOST_125_CM_3" ->
                        succeed VehicleCategoryTricycleMoreThan50Cm3AtMost125Cm3

                    "TRICYCLE_MORE_THAN_125_CM_3" ->
                        succeed VehicleCategoryTricycleMoreThan125Cm3

                    "PERSONAL_TRANSPORTER_MOTORIZED" ->
                        succeed VehicleCategoryPersonalTransporterMotorized

                    "PERSONAL_TRANSPORTER_UNMOTORIZED" ->
                        succeed VehicleCategoryPersonalTransporterUnmotorized

                    "PEDELEC" ->
                        succeed VehicleCategoryPedelec

                    "OTHER" ->
                        succeed VehicleCategoryOther

                    unknown ->
                        fail ("Unknown vehicle category: " ++ unknown)
            )


fixedObstacle : Decoder FixedObstacle
fixedObstacle =
    string
        |> andThen
            (\str ->
                case str of
                    "PARKED_VEHICLE" ->
                        succeed FixedObstacleParkedVehicle

                    "TREE" ->
                        succeed FixedObstacleTree

                    "METAL_SLIDE" ->
                        succeed FixedObstacleMetalSlide

                    "CONCRETE_CHUTE" ->
                        succeed FixedObstacleConcreteChute

                    "OTHER_SLIDE" ->
                        succeed FixedObstacleOtherSlide

                    "BUILDING_WALL_BRIDGE_PIER" ->
                        succeed FixedObstacleBuildingWallBridgePier

                    "VERTICAL_SIGNAL_SUPPORT_EMERGENCY_CALL_STATION" ->
                        succeed FixedObstacleVerticalSignalSupportEmergencyCallStation

                    "POST" ->
                        succeed FixedObstaclePost

                    "STREET_FURNITURE" ->
                        succeed FixedObstacleStreetFurniture

                    "PARAPET" ->
                        succeed FixedObstacleParapet

                    "ISLAND_REFUGE_HIGH_POST" ->
                        succeed FixedObstacleIslandRefugeHighPost

                    "SIDEWALK_CURB" ->
                        succeed FixedObstacleSidewalkCurb

                    "DITCH_EMBANKMENT_ROCK_FACE" ->
                        succeed FixedObstacleDitchEmbankmentRockFace

                    "OTHER_ON_ROAD" ->
                        succeed FixedObstacleOtherOnRoad

                    "OTHER_ON_SIDEWALK_OR_SHOULDER" ->
                        succeed FixedObstacleOtherOnSidewalkOrShoulder

                    "OBSTACLE_FREE_ROADWAY_EXIT" ->
                        succeed FixedObstacleObstacleFreeRoadwayExit

                    "NOZZLE_AQUEDUCT_HEAD" ->
                        succeed FixedObstacleNozzleAqueductHead

                    unknown ->
                        fail ("Unknown fixed obstacle: " ++ unknown)
            )


mobileObstacle : Decoder MobileObstacle
mobileObstacle =
    string
        |> andThen
            (\str ->
                case str of
                    "PEDESTRIAN" ->
                        succeed MobileObstaclePedestrian

                    "VEHICLE" ->
                        succeed MobileObstacleVehicle

                    "RAIL_VEHICLE" ->
                        succeed MobileObstacleRailVehicle

                    "DOMESTIC_ANIMAL" ->
                        succeed MobileObstacleDomesticAnimal

                    "WILD_ANIMAL" ->
                        succeed MobileObstacleWildAnimal

                    "OTHER" ->
                        succeed MobileObstacleOther

                    unknown ->
                        fail ("Unknown mobile obstacle: " ++ unknown)
            )


shockPoint : Decoder ShockPoint
shockPoint =
    string
        |> andThen
            (\str ->
                case str of
                    "FRONT" ->
                        succeed ShockPointFront

                    "FRONT_RIGHT" ->
                        succeed ShockPointFrontRight

                    "FRONT_LEFT" ->
                        succeed ShockPointFrontLeft

                    "REAR" ->
                        succeed ShockPointRear

                    "REAR_RIGHT" ->
                        succeed ShockPointRearRight

                    "REAR_LEFT" ->
                        succeed ShockPointRearLeft

                    "SIDE_RIGHT" ->
                        succeed ShockPointSideRight

                    "SIDE_LEFT" ->
                        succeed ShockPointSideLeft

                    "MULTIPLE_IMPACTS_ROLLOVERS" ->
                        succeed ShockPointMultipleImpactsRollovers

                    unknown ->
                        fail ("Unknown shock point: " ++ unknown)
            )


manoeuvre : Decoder Manoeuvre
manoeuvre =
    string
        |> andThen
            (\str ->
                case str of
                    "NO_CHANGE_OF_DIRECTION" ->
                        succeed ManoeuvreNoChangeOfDirection

                    "SAME_DIRECTION_SAME_LINE" ->
                        succeed ManoeuvreSameDirectionSameLine

                    "BETWEEN_TWO_LINES" ->
                        succeed ManoeuvreBetweenTwoLines

                    "REVERSING" ->
                        succeed ManoeuvreReversing

                    "OPPOSITE_DIRECTION" ->
                        succeed ManoeuvreOppositeDirection

                    "CROSSING_CENTRAL_RESERVATION" ->
                        succeed ManoeuvreCrossingCentralReservation

                    "ON_BUS_LANE_SAME_DIRECTION" ->
                        succeed ManoeuvreOnBusLaneSameDirection

                    "ON_BUS_LANE_OPPOSITE_DIRECTION" ->
                        succeed ManoeuvreOnBusLaneOppositeDirection

                    "MERGING" ->
                        succeed ManoeuvreMerging

                    "TURNING_AROUND_ON_ROAD" ->
                        succeed ManoeuvreTurningAroundOnRoad

                    "CHANGING_LANES_LEFT" ->
                        succeed ManoeuvreChangingLanesLeft

                    "CHANGING_LANES_RIGHT" ->
                        succeed ManoeuvreChangingLanesRight

                    "DEPORTED_LEFT" ->
                        succeed ManoeuvreDeportedLeft

                    "DEPORTED_RIGHT" ->
                        succeed ManoeuvreDeportedRight

                    "TURNING_LEFT" ->
                        succeed ManoeuvreTurningLeft

                    "TURNING_RIGHT" ->
                        succeed ManoeuvreTurningRight

                    "EXCEEDING_LEFT" ->
                        succeed ManoeuvreExceedingLeft

                    "EXCEEDING_RIGHT" ->
                        succeed ManoeuvreExceedingRight

                    "CROSSING_ROADWAY" ->
                        succeed ManoeuvreCrossingRoadway

                    "PARKING_MANOEUVRE" ->
                        succeed ManoeuvreParkingManoeuvre

                    "AVOIDANCE_MANOEUVRE" ->
                        succeed ManoeuvreAvoidanceManoeuvre

                    "DOOR_OPENING" ->
                        succeed ManoeuvreDoorOpening

                    "STOPPED_EXCLUDING_PARKING" ->
                        succeed ManoeuvreStoppedExcludingParking

                    "PARKED_WITH_OCCUPANTS" ->
                        succeed ManoeuvreParkedWithOccupants

                    "DRIVING_ON_SIDEWALK" ->
                        succeed ManoeuvreDrivingOnSidewalk

                    "OTHER" ->
                        succeed ManoeuvreOther

                    unknown ->
                        fail ("Unknown manoeuvre: " ++ unknown)
            )


engine : Decoder Engine
engine =
    string
        |> andThen
            (\str ->
                case str of
                    "HYDROCARBON" ->
                        succeed EngineHydrocarbon

                    "ELECTRIC_HYBRID" ->
                        succeed EngineElectricHybrid

                    "ELECTRIC" ->
                        succeed EngineElectric

                    "HYDROGEN" ->
                        succeed EngineHydrogen

                    "HUMAN" ->
                        succeed EngineHuman

                    "OTHER" ->
                        succeed EngineOther

                    unknown ->
                        fail ("Unknown engine: " ++ unknown)
            )


person : Decoder Person
person =
    succeed Person
        |> required "place" (nullable place)
        |> required "category" personCategory
        |> required "severity" severity
        |> required "sex" sex
        |> required "birth_year" (nullable int)
        |> required "travel_reason" (nullable travelReason)
        |> required "safety_equipment" (list safetyEquipment)
        |> required "pedestrian_location" (nullable pedestrianLocation)
        |> required "pedestrian_action" (nullable pedestrianAction)
        |> required "pedestrian_company" (nullable pedestrianCompany)


vehicle : Decoder Vehicle
vehicle =
    succeed Vehicle
        |> required "vehicle_name" string
        |> required "vehicle_id" (nullable int)
        |> required "traffic_direction" (nullable trafficDirection)
        |> required "vehicle_category" vehicleCategory
        |> required "fixed_obstacle" (nullable fixedObstacle)
        |> required "mobile_obstacle" (nullable mobileObstacle)
        |> required "shock_point" (nullable shockPoint)
        |> required "primary_manoeuvre" (nullable manoeuvre)
        |> required "engine" (nullable engine)
        |> required "occupancy" (nullable int)
        |> required "persons" (list person)


accident : Decoder Accident
accident =
    succeed Accident
        |> required "accident_id" int
        |> required "timestamp" Iso8601.decoder
        |> required "latitude" (nullable float)
        |> required "longitude" (nullable float)
        |> required "address" string
        |> required "light" (nullable light)
        |> required "intersection" (nullable intersection)
        |> required "atmospheric_conditions" (nullable atmosphericConditions)
        |> required "collision" (nullable collision)
        |> required "location" (nullable locationRegime)
        |> required "department" string
        |> required "commune" string
        |> required "road_category" roadCategory
        |> required "road" string
        |> required "road_index_number" (nullable int)
        |> required "road_index_alpha" (nullable string)
        |> required "traffic_regime" (nullable trafficRegime)
        |> required "lanes_count" (nullable int)
        |> required "dedicated_lane" (nullable dedicatedLane)
        |> required "profile" (nullable profile)
        |> required "upstream_terminal" (nullable int)
        |> required "upstream_terminal_distance_meters" (nullable float)
        |> required "curvature" (nullable curvature)
        |> required "central_reservation_width_meters" int
        |> required "road_traffic_width_meters" float
        |> required "vehicles"
            (list vehicle)


expectStringLines : (Result Http.Error (List String) -> msg) -> Http.Expect msg
expectStringLines toMsg =
    Http.expectString
        (\result ->
            toMsg (Result.map String.lines result)
        )


type HttpJsonError
    = HttpError Http.Error
    | JsonDecodeError Json.Decode.Error


expectJsonLines : (Result HttpJsonError (List a) -> msg) -> Decoder a -> Http.Expect msg
expectJsonLines toMsg decoder =
    expectStringLines
        (\result ->
            result
                |> Result.mapError HttpError
                |> Result.andThen
                    (\lines ->
                        lines
                            |> List.foldl
                                (\line results ->
                                    line
                                        |> decodeString decoder
                                        |> Result.mapError JsonDecodeError
                                        |> Result.map2 (\xs x -> List.append xs [ x ]) results
                                )
                                (Ok [])
                    )
                |> toMsg
        )


expectAccidentJsonLines : (Result HttpJsonError (List Accident) -> msg) -> Http.Expect msg
expectAccidentJsonLines toMsg =
    expectJsonLines toMsg accident
