from datetime import datetime
from enum import IntEnum
from typing import Optional, NamedTuple


class Light(IntEnum):
    DAYLIGHT = 1
    DUSK_OR_DAWN = 2
    NIGHT_WITHOUT_PUBLIC_LIGHTING = 3
    NIGHT_WITH_PUBLIC_LIGHTING_OFF = 4
    NIGHT_WITH_PUBLIC_LIGHTING_ON = 5


class Intersection(IntEnum):
    OUT_OF_INTERSECTION = 1
    X_INTERSECTION = 2
    T_INTERSECTION = 3
    Y_INTERSECTION = 4
    INTERSECTION_WITH_MORE_THAN_4_BRANCHES = 5
    ROUNDABOUT = 6
    PLACE = 7
    LEVEL_CROSSING = 8
    OTHER_INTERSECTION = 9


class AtmosphericConditions(IntEnum):
    NORMAL = 1
    LIGHT_RAIN = 2
    HEAVY_RAIN = 3
    SNOW_HAIL = 4
    FOG_SMOKE = 5
    STRONG_WIND_STORM = 6
    DAZZLING_WEATHER = 7
    OVERCAST_WEATHER = 8
    OTHER = 9


class Collision(IntEnum):
    TWO_VEHICLES_FRONT = 1
    TWO_VEHICLES_FROM_THE_REAR = 2
    TWO_VEHICLES_FROM_THE_SIDE = 3
    THREE_OR_MORE_VEHICLES_IN_A_CHAIN = 4
    THREE_OR_MORE_VEHICLES_MULTIPLE_COLLISIONS = 5
    OTHER_COLLISION = 6
    WITHOUT_COLLISION = 7


class LocationRegime(IntEnum):
    OUT_OF_TOWN = 1
    IN_BUILT_UP_AREAS = 2


class Characteristic(NamedTuple):
    accident_id: int
    timestamp: datetime
    latitude: Optional[float]
    longitude: Optional[float]
    address: str
    light: Optional[Light]
    intersection: Optional[Intersection]
    atmospheric_conditions: Optional[AtmosphericConditions]
    collision: Optional[Collision]
    location: Optional[LocationRegime]
    department: str
    commune: str

    def __int__(self):
        assert self.accident_id >= 0
        assert self.latitude is None or -90 <= self.latitude <= 90
        assert self.longitude is None or -180 <= self.longitude <= 180


class RoadCategory(IntEnum):
    HIGHWAY = 1
    NATIONAL_ROAD = 2
    DEPARTMENTAL_ROAD = 3
    MUNICIPAL_ROADS = 4
    OFF_THE_PUBLIC_NETWORK = 5
    PARKING_LOT_OPEN_TO_PUBLIC_TRAFFIC = 6
    URBAN_METROPOLITAN_ROADS = 7
    OTHER = 9


class TrafficRegime(IntEnum):
    ONE_WAY = 1
    BIDIRECTIONAL = 2
    WITH_SEPARATE_LANES = 3
    WITH_VARIABLE_ASSIGNMENT_LANES = 4


class DedicatedLane(IntEnum):
    NONE = 0
    BICYCLE_PATH = 1
    CYCLE_LANE = 2
    RESERVED_LANE = 3


class Profile(IntEnum):
    FLAT = 1
    SLOPE = 2
    TOP_OF_HILL = 3
    BOTTOM_OF_HILL = 4


class Curvature(IntEnum):
    STRAIGHT = 1
    LEFT_HAND_CURVE = 2
    RIGHT_HAND_CURVE = 3
    S_CURVE = 4


class Location(NamedTuple):
    accident_id: int
    road_category: RoadCategory
    road: str
    road_index_number: Optional[int]
    road_index_alpha: Optional[str]
    traffic_regime: Optional[TrafficRegime]
    lanes_count: Optional[int]
    dedicated_lane: Optional[DedicatedLane]
    profile: Optional[Profile]
    upstream_terminal: Optional[int]
    upstream_terminal_distance_meters: Optional[float]
    curvature: Optional[Curvature]
    central_reservation_width_meters: int
    road_traffic_width_meters: float

    def __int__(self):
        assert self.accident_id >= 0
        assert self.road_index_number >= 0
        assert self.lanes_count >= 0
        assert self.upstream_terminal >= 0
        assert self.upstream_terminal_distance_meters >= 0
        assert self.central_reservation_width_meters >= 0
        assert self.road_traffic_width_meters >= 0


class Accident(NamedTuple):
    accident_id: int
    characteristic: Characteristic
    location: Location
