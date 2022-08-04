from collections import defaultdict
from pathlib import Path

from cache import cache_artifacts, DATA_DIR
from model import Accident, Vehicle, Person, Location, AccidentId, VehicleId, \
    VehicleCategory, Characteristic
from parse.accident import AccidentsJsonlFormatter
from parse.characteristics import CharacteristicsCsvParser
from parse.locations import LocationsCsvParser
from parse.person import PersonsCsvParser
from parse.vehicle import VehiclesCsvParser


def _matches_file(file: Path, prefix: str) -> bool:
    stem = file.stem
    if not file.name.startswith(prefix):
        return False
    stem = stem.removeprefix(prefix)
    stem = stem.removeprefix("-")
    return stem.isnumeric()


def _matching_files(files: list[Path], prefix: str) -> list[Path]:
    return [
        file
        for file in files
        if _matches_file(file, prefix)
    ]


def main() -> None:
    files: list[Path] = cache_artifacts()
    characteristics = CharacteristicsCsvParser().parse(
        _matching_files(files, "caracteristiques")
    )
    locations = LocationsCsvParser().parse(_matching_files(files, "lieux"))
    vehicles = VehiclesCsvParser().parse(_matching_files(files, "vehicules"))
    persons = PersonsCsvParser().parse(_matching_files(files, "usagers"))

    accident_characteristics: dict[AccidentId, Characteristic] = {
        accident_id: characteristic
        for accident_id, characteristic in characteristics
    }
    accident_locations: dict[AccidentId, Location] = {
        accident_id: location
        for accident_id, location in locations
    }
    accident_vehicles: dict[AccidentId, dict[VehicleId, Vehicle]] = (
        defaultdict(lambda: {})
    )
    for accident_id, vehicle_id, vehicle in vehicles:
        accident_vehicles[accident_id][vehicle_id] = vehicle
    accident_persons: dict[AccidentId, dict[VehicleId, list[Person]]] = (
        defaultdict(lambda: defaultdict(lambda: []))
    )
    for accident_id, vehicle_id, person in persons:
        accident_persons[accident_id][vehicle_id].append(person)

    accident_ids_characteristics = set(accident_characteristics.keys())
    accident_ids_locations = set(accident_locations.keys())
    assert accident_ids_locations == accident_ids_characteristics
    accident_ids_vehicles = set(accident_vehicles.keys())
    assert accident_ids_vehicles.issubset(accident_characteristics)
    accident_ids_persons = set(accident_persons.keys())
    assert accident_ids_persons.issubset(accident_ids_vehicles)

    for accident_id, persons_dict in accident_persons.items():
        vehicles_dict = accident_vehicles[accident_id]
        persons_vehicle_ids = set(persons_dict.keys())
        vehicle_ids = set(vehicles_dict.keys())
        if not persons_vehicle_ids.issubset(vehicle_ids):
            for vehicle_id in persons_vehicle_ids - vehicle_ids:
                # Create new vehicle entry.
                accident_vehicles[accident_id][vehicle_id] = Vehicle(
                    *vehicle_id,
                    traffic_direction=None,
                    vehicle_category=VehicleCategory.OTHER,
                    fixed_obstacle=None,
                    mobile_obstacle=None,
                    shock_point=None,
                    primary_manoeuvre=None,
                    engine=None,
                    occupancy=None,
                    persons=[]
                )

    accidents = [
        Accident(
            *accident_id,
            *accident_characteristics[accident_id],
            *accident_locations[accident_id],
            vehicles=[
                vehicle._replace(
                    persons=accident_persons[accident_id][vehicle_id]
                )
                for vehicle_id, vehicle in (
                    accident_vehicles[accident_id].items()
                )
            ],
        )
        for accident_id in accident_ids_characteristics
    ]
    AccidentsJsonlFormatter().format(accidents, DATA_DIR)


if __name__ == "__main__":
    main()
