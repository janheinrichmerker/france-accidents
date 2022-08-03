from pathlib import Path

from cache import cache_artifacts, DATA_DIR
from model import Accident
from parse.accident import AccidentsJsonlFormatter
from parse.characteristics import CharacteristicsCsvParser
from parse.locations import LocationsCsvParser
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
    print(sum(1 for _ in characteristics))


if __name__ == "__main__":
    main()
