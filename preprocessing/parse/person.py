from csv import DictReader
from itertools import chain
from pathlib import Path
from typing import Iterable, FrozenSet, Tuple

from tqdm.auto import tqdm

from model import (
    Person, Place, PersonCategory, Severity, Sex,
    TravelReason, SafetyEquipment, PedestrianLocation, PedestrianAction,
    PedestrianCompany, AccidentId, VehicleId
)
from parse import Parser
from parse.util import count_lines


class PersonsCsvParser(Parser[Tuple[AccidentId, VehicleId, Person]]):

    @staticmethod
    def _equipment(
            value: str
    ) -> FrozenSet[SafetyEquipment]:
        if value in {"0", "-1", "11"}:
            return frozenset()
        elif value == "8":
            return frozenset({SafetyEquipment.OTHER})
        elif value == "7":
            return frozenset({SafetyEquipment.AIRBAG, SafetyEquipment.GLOVES})
        else:
            return frozenset({SafetyEquipment(int(value))})

    @staticmethod
    def _parse_file(
            path: Path,
            progress: tqdm,
    ) -> Iterable[Tuple[AccidentId, VehicleId, Person]]:
        file_year = int(path.name.split("-")[-1].removesuffix(".csv"))
        delimiter: str
        if file_year >= 2019:
            delimiter = ";"
        else:
            delimiter = ","
        with path.open("r") as file:
            reader = DictReader(file, delimiter=delimiter, quotechar='"')
            for row in reader:
                for key in row:
                    row[key] = str(row[key]).strip()
                yield (
                    AccidentId(
                        accident_id=int(row["Num_Acc"]),
                    ),
                    VehicleId(
                        vehicle_id=(
                            int(str(row["id_vehicule"]).replace(" ", ""))
                            if "id_vehicule" in row else None
                        ),
                        vehicle_name=str(row["num_veh"]),
                    ),
                    Person(
                        place=(
                            Place(int(row["place"]))
                            if row["place"] not in {"", "0"} else None
                        ),
                        category=(
                            PersonCategory(int(row["catu"]))
                            if row["catu"] != "4" else None
                        ),
                        severity=Severity(int(row["grav"])),
                        sex=Sex(int(row["sexe"])),
                        birth_year=(
                            int(row["an_nais"])
                            if row["an_nais"] != "" else None
                        ),
                        travel_reason=(
                            TravelReason(int(row["trajet"]))
                            if row["trajet"] not in {"", "0", "-1"} else None
                        ),
                        safety_equipment=(
                            set(chain.from_iterable(
                                (
                                    PersonsCsvParser._equipment(str(char))
                                    for char in str(row["secu"])
                                )
                                if "secu" in row else
                                (
                                    PersonsCsvParser._equipment(row["secu1"]),
                                    PersonsCsvParser._equipment(row["secu2"]),
                                    PersonsCsvParser._equipment(row["secu3"]),
                                )
                            ))
                        ),
                        pedestrian_location=(
                            PedestrianLocation(int(row["locp"]))
                            if row["locp"] not in {
                                "", "0", "-1", "9"
                            } else None
                        ),
                        pedestrian_action=(
                            PedestrianAction.GETTING_ON_OFF_VEHICLE
                            if row["actp"] == "A" else
                            PedestrianAction(int(row["actp"]))
                            if row["actp"] not in {
                                "", "0", "-1", "7", "8", "B"
                            } else None
                        ),
                        pedestrian_company=(
                            PedestrianCompany(int(row["etatp"]))
                            if row["etatp"] not in {"", "0", "-1"} else None
                        ),
                    )
                )
                progress.update(1)

    def parse(
            self,
            input_paths: list[Path]
    ) -> Iterable[Tuple[AccidentId, VehicleId, Person]]:
        progress = tqdm(
            desc="Parsing persons",
            total=count_lines(input_paths) - len(input_paths),
            unit="line",
        )
        for path in input_paths:
            yield from self._parse_file(path, progress)
