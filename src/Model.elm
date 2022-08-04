module Model exposing (..)

import Date exposing (Date)
import Set exposing (Set)


type Light
    = LightDaylight
    | LightDuskOrDawn
    | LightNightWithoutPublicLighting
    | LightNightWithPublicLightingOff
    | LightNightWithPublicLightingOn


type Intersection
    = IntersectionOutOfIntersection
    | IntersectionXIntersection
    | IntersectionTIntersection
    | IntersectionYIntersection
    | IntersectionIntersectionWithMoreThan4Branches
    | IntersectionRoundabout
    | IntersectionPlace
    | IntersectionLevelCrossing
    | IntersectionOtherIntersection


type AtmosphericConditions
    = AtmosphericConditionsNormal
    | AtmosphericConditionsLightRain
    | AtmosphericConditionsHeavyRain
    | AtmosphericConditionsSnowHail
    | AtmosphericConditionsFogSmoke
    | AtmosphericConditionsStrongWindStorm
    | AtmosphericConditionsDazzlingWeather
    | AtmosphericConditionsOvercastWeather
    | AtmosphericConditionsOther


type Collision
    = CollisionTwoVehiclesFront
    | CollisionTwoVehiclesFromTheRear
    | CollisionTwoVehiclesFromTheSide
    | CollisionThreeOrMoreVehiclesInAChain
    | CollisionThreeOrMoreVehiclesMultipleCollisions
    | CollisionOtherCollision
    | CollisionWithoutCollision


type LocationRegime
    = LocationRegimeOutOfTown
    | LocationRegimeInBuiltUpAreas


type alias Characteristic =
    { timestamp : Date
    , latitude : Maybe Float
    , longitude : Maybe Float
    , address : String
    , light : Maybe Light
    , intersection : Maybe Intersection
    , atmospheric_conditions : Maybe AtmosphericConditions
    , collision : Maybe Collision
    , location : Maybe LocationRegime
    , department : String
    , commune : String
    }


type RoadCategory
    = RoadCategoryHighway
    | RoadCategoryNationalRoad
    | RoadCategoryDepartmentalRoad
    | RoadCategoryMunicipalRoads
    | RoadCategoryOffThePublicNetwork
    | RoadCategoryParkingLotOpenToPublicTraffic
    | RoadCategoryUrbanMetropolitanRoads
    | RoadCategoryOther


type TrafficRegime
    = TrafficRegimeOneWay
    | TrafficRegimeBidirectional
    | TrafficRegimeWithSeparateLanes
    | TrafficRegimeWithVariableAssignmentLanes


type DedicatedLane
    = DedicatedLaneNone
    | DedicatedLaneBicyclePath
    | DedicatedLaneCycleLane
    | DedicatedLaneReservedLane


type Profile
    = ProfileFlat
    | ProfileSlope
    | ProfileTopOfHill
    | ProfileBottomOfHill


type Curvature
    = CurvatureStraight
    | CurvatureLeftHandCurve
    | CurvatureRightHandCurve
    | CurvatureSCurve


type alias Location =
    { road_category : RoadCategory
    , road : String
    , road_index_number : Maybe Int
    , road_index_alpha : Maybe String
    , traffic_regime : Maybe TrafficRegime
    , lanes_count : Maybe Int
    , dedicated_lane : Maybe DedicatedLane
    , profile : Maybe Profile
    , upstream_terminal : Maybe Int
    , upstream_terminal_distance_meters : Maybe Float
    , curvature : Maybe Curvature
    , central_reservation_width_meters : Int
    , road_traffic_width_meters : Float
    }


type Place
    = PlaceFrontLeftMotorcycleFront
    | PlaceFrontRightMotorcycleRear
    | PlaceRearRightMotorcycleSidecar
    | PlaceRearLeft
    | PlaceRearCenter
    | PlaceFrontCenter
    | PlaceCenterLeft
    | PlaceCenterMiddle
    | PlaceCenterRight
    | PlacePedestrian


type PersonCategory
    = PersonCategoryDriver
    | PersonCategoryPassenger
    | PersonCategoryPedestrian


type Severity
    = SeverityUnharmed
    | SeverityKilled
    | SeverityInjuredHospitalized
    | SeveritySlightlyInjured


type Sex
    = SexFemale
    | SexMale


type TravelReason
    = TravelReasonHomeToWork
    | TravelReasonHomeToSchool
    | TravelReasonShopping
    | TravelReasonProfessional
    | TravelReasonWalkingLeisure
    | TravelReasonOther


type SafetyEquipment
    = SafetyEquipmentBelt
    | SafetyEquipmentHeadset
    | SafetyEquipmentChildrenDevice
    | SafetyEquipmentReflectiveVest
    | SafetyEquipmentAirbag
    | SafetyEquipmentGloves
    | SafetyEquipmentOther


type PedestrianLocation
    = PedestrianLocationOnPavementAtLeast50MFromPedestrianCrossing
    | PedestrianLocationOnPavementAtMost50MFromPedestrianCrossing
    | PedestrianLocationOnPedestrianCrossingWithoutLightSignal
    | PedestrianLocationOnPedestrianCrossingWithLightSignal
    | PedestrianLocationOnSidewalk
    | PedestrianLocationOnRoadShoulder
    | PedestrianLocationOnEmergencyBayOrShoulder
    | PedestrianLocationOnCounterAisle


type PedestrianAction
    = PedestrianActionDirectionOfImpactingVehicle
    | PedestrianActionOppositeDirectionOfImpactingVehicle
    | PedestrianActionCrossing
    | PedestrianActionMasked
    | PedestrianActionPlayingRunning
    | PedestrianActionWithAnimal
    | PedestrianActionOther
    | PedestrianActionGettingOnOffVehicle


type PedestrianCompany
    = PedestrianCompanyAlone
    | PedestrianCompanyAccompanied
    | PedestrianCompanyGroup


type alias Person =
    { place : Maybe Place
    , category : PersonCategory
    , severity : Severity
    , sex : Sex
    , birth_year : Maybe Int
    , travel_reason : Maybe TravelReason
    , safety_equipment : Set SafetyEquipment
    , pedestrian_location : Maybe PedestrianLocation
    , pedestrian_action : Maybe PedestrianAction
    , pedestrian_company : Maybe PedestrianCompany
    }


type TrafficDirection
    = TrafficDirectionIncreasingReferenceNumber
    | TrafficDirectionDecreasingReferenceNumber
    | TrafficDirectionNoReference


type VehicleCategory
    = VehicleCategoryBicycle
    | VehicleCategoryMopedLessThan50Cm3
    | VehicleCategoryQuadWithBody
    | VehicleCategoryRegisteredScooter
    | VehicleCategoryMotorcycle
    | VehicleCategorySidecar
    | VehicleCategoryLightVehicleOnly
    | VehicleCategoryLightVehicleWithCaravan
    | VehicleCategoryLightVehicleWithTrailer
    | VehicleCategoryCommercialVehicleOnlyWeightRatingAtLeast1_5TAtMost3_5T
    | VehicleCategoryCommercialVehicleWithCaravanWeightRatingAtLeast1_5TAtMost3_5T
    | VehicleCategoryCommercialVehicleWithTrailerWeightRatingAtLeast1_5TAtMost3_5T
    | VehicleCategoryTruckOnlyWeightRatingMoreThan3_5TAtMost7_5T
    | VehicleCategoryTruckOnlyWeightRatingMoreThan7_5T
    | VehicleCategoryTruckMoreThan3_5TWithTrailer
    | VehicleCategoryRoadTractorOnly
    | VehicleCategoryRoadTractorWithSemiTrailer
    | VehicleCategoryPublicTransport
    | VehicleCategorySpecialMachine
    | VehicleCategoryAgriculturalTractor
    | VehicleCategoryScooterLessThan50Cm3
    | VehicleCategoryMotorcycleMoreThan50Cm3AtMost125Cm3
    | VehicleCategoryScooterMoreThan50Cm3AtMost125Cm3
    | VehicleCategoryMotorcycleMoreThan125Cm3
    | VehicleCategoryScooterMoreThan125Cm3
    | VehicleCategoryQuadWithoutBodyAtMost50Cm3
    | VehicleCategoryQuadWithoutBodyMoreThan50Cm3
    | VehicleCategoryBuses
    | VehicleCategoryBus
    | VehicleCategoryTrain
    | VehicleCategoryTramway
    | VehicleCategoryTricycleAtMost50Cm3
    | VehicleCategoryTricycleMoreThan50Cm3AtMost125Cm3
    | VehicleCategoryTricycleMoreThan125Cm3
    | VehicleCategoryPersonalTransporterMotorized
    | VehicleCategoryPersonalTransporterUnmotorized
    | VehicleCategoryPedelec
    | VehicleCategoryOther


type FixedObstacle
    = FixedObstacleParkedVehicle
    | FixedObstacleTree
    | FixedObstacleMetalSlide
    | FixedObstacleConcreteChute
    | FixedObstacleOtherSlide
    | FixedObstacleBuildingWallBridgePier
    | FixedObstacleVerticalSignalSupportEmergencyCallStation
    | FixedObstaclePost
    | FixedObstacleStreetFurniture
    | FixedObstacleParapet
    | FixedObstacleIslandRefugeHighPost
    | FixedObstacleSidewalkCurb
    | FixedObstacleDitchEmbankmentRockFace
    | FixedObstacleOtherOnRoad
    | FixedObstacleOtherOnSidewalkOrShoulder
    | FixedObstacleObstacleFreeRoadwayExit
    | FixedObstacleNozzleAqueductHead


type MobileObstacle
    = MobileObstaclePedestrian
    | MobileObstacleVehicle
    | MobileObstacleRailVehicle
    | MobileObstacleDomesticAnimal
    | MobileObstacleWildAnimal
    | MobileObstacleOther


type ShockPoint
    = ShockPointFront
    | ShockPointFrontRight
    | ShockPointFrontLeft
    | ShockPointRear
    | ShockPointRearRight
    | ShockPointRearLeft
    | ShockPointSideRight
    | ShockPointSideLeft
    | ShockPointMultipleImpactsRollovers


type Manoeuvre
    = ManoeuvreNoChangeOfDirection
    | ManoeuvreSameDirectionSameLine
    | ManoeuvreBetweenTwoLines
    | ManoeuvreReversing
    | ManoeuvreOppositeDirection
    | ManoeuvreCrossingCentralReservation
    | ManoeuvreOnBusLaneSameDirection
    | ManoeuvreOnBusLaneOppositeDirection
    | ManoeuvreMerging
    | ManoeuvreTurningAroundOnRoad
    | ManoeuvreChangingLanesLeft
    | ManoeuvreChangingLanesRight
    | ManoeuvreDeportedLeft
    | ManoeuvreDeportedRight
    | ManoeuvreTurningLeft
    | ManoeuvreTurningRight
    | ManoeuvreExceedingLeft
    | ManoeuvreExceedingRight
    | ManoeuvreCrossingRoadway
    | ManoeuvreParkingManoeuvre
    | ManoeuvreAvoidanceManoeuvre
    | ManoeuvreDoorOpening
    | ManoeuvreStoppedExcludingParking
    | ManoeuvreParkedWithOccupants
    | ManoeuvreDrivingOnSidewalk
    | ManoeuvreOther


type Engine
    = EngineHydrocarbon
    | EngineElectricHybrid
    | EngineElectric
    | EngineHydrogen
    | EngineHuman
    | EngineOther


type alias Vehicle =
    { vehicle_name : String
    , vehicle_id : Maybe Int
    , traffic_direction : Maybe TrafficDirection
    , vehicle_category : VehicleCategory
    , fixed_obstacle : Maybe FixedObstacle
    , mobile_obstacle : Maybe MobileObstacle
    , shock_point : Maybe ShockPoint
    , primary_manoeuvre : Maybe Manoeuvre
    , engine : Maybe Engine
    , occupancy : Maybe Int
    , persons : List Person
    }


type alias Accident =
    { accident_id : Int
    , timestamp : Date
    , latitude : Maybe Float
    , longitude : Maybe Float
    , address : String
    , light : Maybe Light
    , intersection : Maybe Intersection
    , atmospheric_conditions : Maybe AtmosphericConditions
    , collision : Maybe Collision
    , location : Maybe LocationRegime
    , department : String
    , commune : String
    , road_category : RoadCategory
    , road : String
    , road_index_number : Maybe Int
    , road_index_alpha : Maybe String
    , traffic_regime : Maybe TrafficRegime
    , lanes_count : Maybe Int
    , dedicated_lane : Maybe DedicatedLane
    , profile : Maybe Profile
    , upstream_terminal : Maybe Int
    , upstream_terminal_distance_meters : Maybe Float
    , curvature : Maybe Curvature
    , central_reservation_width_meters : Int
    , road_traffic_width_meters : Float
    , vehicles : List Vehicle
    }
