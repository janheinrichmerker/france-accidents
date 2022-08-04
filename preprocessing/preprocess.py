from collections import defaultdict
from pathlib import Path

from cache import cache_artifacts, DATA_DIR
from model import Accident, Vehicle, Person, Characteristic, Location
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
    for _ in persons:
        pass

    accident_characteristics: dict[int, Characteristic] = {
        characteristic.accident_id: characteristic
        for characteristic in characteristics
    }
    accident_locations: dict[int, Location] = {
        location.accident_id: location
        for location in locations
    }
    accident_vehicles: dict[int, list[Vehicle]] = defaultdict(lambda: [])
    for vehicle in vehicles:
        accident_vehicles[vehicle.accident_id].append(vehicle)
    accident_persons: dict[int, list[Person]] = defaultdict(lambda: [])
    for person in persons:
        accident_persons[person.accident_id].append(person)

    accident_ids = (
            set(accident_characteristics.keys()) |
            set(accident_locations.keys()) |
            set(accident_vehicles.keys()) |
            set(accident_persons.keys())
    )
    accidents = [
        Accident(
            accident_id,
            *accident_characteristics[accident_id][1:],
            *accident_locations[accident_id][1:],
            vehicles=accident_vehicles[accident_id],
            persons=accident_persons[accident_id],
        )
        for accident_id in accident_ids
    ]
    AccidentsJsonlFormatter().format(accidents, DATA_DIR)


if __name__ == "__main__":
    main()
