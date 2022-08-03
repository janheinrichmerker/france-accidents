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


class Accident(Characteristic):
    pass
