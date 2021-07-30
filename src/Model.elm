module Model exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Posix)


type alias Accident =
    { id : AccidentId
    , characteristic : Characteristic
    , place : Place
    , vehicles : Set Vehicle
    }


type alias Characteristic =
    { dateTime : Posix
    , lighting : Lighting
    , departement : Departement
    , muncipality : Muncipality
    , localisation : Localisation
    , intersection : Intersection
    , weather : Weather
    , collision : Collision
    , address : Address
    , geoOriginator : GeoOriginator
    , coordinates: Coordinates
    }


type alias AccidentId =
    String


type Lighting
    = LightingFullDay
    | LightingTwilightDawn
    | LightingNightWithoutPublicLighting
    | LightingNightWithPublicLightingNotLit
    | LightingNightWithPublicLighting


type alias Departement =
    Int


type alias Muncipality =
    Int


type Localisation
    = LocalisationOutOfAgglomeration
    | LocalisationInBuiltUpAreas


type Intersection
    = IntersectionOutOfIntersection
    | IntersectionX
    | IntersectionT
    | IntersectionY
    | IntersectionMoreThan4Branches
    | IntersectionGiratory
    | IntersectionPlace
    | IntersectionLevelCrossing
    | IntersectionOther


type Weather
    = WeatherNormal
    | WeatherLightRain
    | WeatherHeavyRain
    | WeatherSnowHail
    | WeatherFogSmoke
    | WeatherStrongWindStorm
    | WeatherDazzling
    | WeatherCloudy
    | WeatherOther


type Collision
    = CollisionTwoVehiclesFrontal
    | CollisionTwoVehiclesRear
    | CollisionTwoVehiclesSide
    | CollisionThreeOrMoreVehiclesChain
    | CollisionThreeOrMoreVehiclesMultipleCollisions
    | CollisionOther
    | CollisionNone


type alias Address =
    String


type GeoOriginator
    = GeoOriginatorMetropole
    | GeoOriginatorAntilles
    | GeoOriginatorGuyane
    | GeoOriginatorReunion
    | GeoOriginatorMayotte


type alias Coordinates =
    { latitude : Latitude
    , longitude : Longitude
    }

type alias Latitude =
    Float


type alias Longitude =
    Float


type alias Place =
    { road : Road
    , roadNumber : RoadNumber
    , roadNumberIndex : RoadNumberIndex
    , roadNumberIndexLetter : RoadNumberIndexLetter
    , trafficRegime : TrafficRegime
    , numberLanes : NumberLanes
    , reservedLane : ReservedLane
    , profile : Profile
    , upstreamTerminalNumber : UpstreamTerminalNumber
    , upstreamTerminalDistanceMeters : UpstreamTerminalDistanceMeters
    , curvature : Curvature
    , centralReservationWidth : CentralReservationWidth
    , roadwayTrafficWidth : RoadwayTrafficWidth
    , surfaceCondition : SurfaceCondition
    , infrastructure : Infrastructure
    , situation : Situation
    , schoolPoint : SchoolPoint
    }


type Road
    = RoadHighway
    | RoadNationalRoad
    | RoadDepartmentalRoad
    | RoadCommunalWay
    | RoadOffPublicNetwork
    | RoadPublicParking
    | RoadOther


type alias RoadNumber =
    Int


type alias RoadNumberIndex =
    String


type alias RoadNumberIndexLetter =
    String


type TrafficRegime
    = TrafficRegimeOneWay
    | TrafficRegimeBidirectional
    | TrafficRegimeSeparatedLanes
    | TrafficRegimeVariableAssignmentLanes


type alias NumberLanes =
    Int


type ReservedLane
    = ReservedLaneBikePath
    | ReservedLaneCycleBank
    | ReservedLaneReservedChannel


type Profile
    = ProfileFlat
    | ProfileSloped
    | ProfileHillTop
    | ProfileHillBottom


type alias UpstreamTerminalNumber =
    Int


type alias UpstreamTerminalDistanceMeters =
    Int


type Curvature
    = CurvatureStraight
    | CurvatureCurvedLeft
    | CurvatureCurvedRight
    | CurvatureCurvedS


type alias CentralReservationWidth =
    Int


type alias RoadwayTrafficWidth =
    Int


type SurfaceCondition
    = SurfaceConditionNormal
    | SurfaceConditionWet
    | SurfaceConditionPuddles
    | SurfaceConditionFlooded
    | SurfaceConditionSnow
    | SurfaceConditionMud
    | SurfaceConditionIcy
    | SurfaceConditionFatOil
    | SurfaceConditionOther


type Infrastructure
    = InfrastructureUndergroundOrTunnel
    | InfrastructureBridgeOrOverpass
    | InfrastructureExchangerOrConnectionBrace
    | InfrastructureRailway
    | InfrastructureCrossroads
    | InfrastructurePedestrianArea
    | InfrastructureTollZone


type Situation
    = SituationOnRoad
    | SituationOnHardShoulder
    | SituationOnVerge
    | SituationOnSidewalk
    | SituationOnBikePath


type alias SchoolPoint =
    Int


type alias Person =
    { id : PersonId
    , placeId : PlaceId
    , personCategory : PersonType
    , severity : Severity
    , sex : Sex
    , birthYear : BirthYear
    , occasion : Occasion
    , safetyEquipment : SafetyEquipment
    , safetyEquipmentUsage : SafetyEquipmentUsage
    , pedestrianLocation : PedestrianLocation
    , action : Action
    , accompaniment : Accompaniment
    }


type alias VehicleId =
    Int


type alias PersonId =
    Int


type alias PlaceId =
    Int


type PersonType
    = PersonTypeDriver
    | PersonTypePassenger
    | PersonTypePedestrian
    | PersonTypePedestrianRollerbladeScooter


type Severity
    = SeverityUnscathed
    | SeverityKilled
    | SeverityHospitalizedWounded
    | SeverityLightInjury


type Sex
    = SexFemale
    | SexMale


type BirthYear
    = Int


type Occasion
    = OccasionHomeWork
    | OccasionHomeSchool
    | OccasionShoppingShopping
    | OccasionProfessional
    | OccasionPromenadeLeisure
    | OccasionOther


type SafetyEquipment
    = SafetyEquipmentBelt
    | SafetyEquipmentHelmet
    | SafetyEquipmentChildrenSeat
    | SafetyEquipmentReflectors
    | SafetyEquipmentOther


type alias SafetyEquipmentUsage =
    Bool


type PedestrianLocation
    = PedestrianLocationPavementFarFromPedestrianCrossing
    | PedestrianLocationPavementNearPedestrianCrossing
    | PedestrianLocationOnPedestrianCrossingWithoutSignals
    | PedestrianLocationOnPedestrianCrossingWithSignals
    | PedestrianLocationOnSidewalk
    | PedestrianLocationOnVerge
    | PedestrianLocationOnRefuge
    | PedestrianLocationOnAgainstAisle


type Action
    = ActionMovingUnspecified
    | ActionMovingBumpingVehicle
    | ActionMovingOppositeDirection
    | ActionCrossing
    | ActionMasked
    | ActionPlayingRunning
    | ActionWithAnimal
    | ActionOther


type Accompaniment
    = AccompanimentAlone
    | AccompanimentAccompanied
    | AccompanimentInGroup


type alias Vehicle =
    { persons : Dict PlaceId Person
    , id : VehicleId
    , flowDirection : FlowDirection
    , vehicleType : VehicleType
    }


type FlowDirection
    = FlowDirectionIncreasingPostalAddressNumber
    | FlowDirectionDescendingPostalAddressNumber


type VehicleType
    = VehicleTypeBicycle
    | VehicleTypeMoped
    | VehicleTypeCart
    | VehicleTypeRegisteredScooter
    | VehicleTypeMotorcycle
    | VehicleTypeSideCar
    | VehicleTypeVl
    | VehicleTypeVlWithCaravan
    | VehicleTypeVlWithTrailer
    | VehicleTypeVu
    | VehicleTypeVuWithCaravan
    | VehicleTypeVuWithTrailer
    | VehicleTypePl
