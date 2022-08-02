from abc import ABC, abstractmethod
from pathlib import Path


class Parser(ABC):
    @abstractmethod
    def parse(self, input_files: list[Path], output_dir: Path) -> None:
        pass


class CharacteristicsParser(Parser):
    def parse(self, input_files: list[Path], output_dir: Path) -> None:
        raise NotImplementedError()


class PassengersParser(Parser):
    def parse(self, input_files: list[Path], output_dir: Path) -> None:
        raise NotImplementedError()


class VehicleParser(Parser):
    def parse(self, input_files: list[Path], output_dir: Path) -> None:
        raise NotImplementedError()


PARSERS: dict[str, Parser] = {
    "caracteristiques": CharacteristicsParser(),
    # "lieux": TODO,
    "usagers": PassengersParser(),
    "vehicules": VehicleParser(),
    # "vehicules-immatricules-baac": TODO,
}
