from csv import DictReader
from pathlib import Path
from typing import Iterable

from tqdm.auto import tqdm

from model import (
    Vehicle, TrafficDirection, VehicleCategory, FixedObstacle,
    MobileObstacle, ShockPoint, Manoeuvre, Engine
)
from parse import Parser
from parse.util import count_lines


class VehiclesCsvParser(Parser[Vehicle]):

    @staticmethod
    def _parse_file(
            path: Path,
            progress: tqdm,
    ) -> Iterable[Vehicle]:
        file_year = int(path.name.split("-")[-1].removesuffix(".csv"))
        delimiter: str
        if file_year >= 2019:
            delimiter = ";"
        else:
            delimiter = ","
        with path.open("r") as file:
            reader = DictReader(file, delimiter=delimiter, quotechar='"')
            for row in reader:
                try:
                    for key in row:
                        row[key] = str(row[key]).strip()
                    if row["catv"] == "19":
                        row["catv"] = "40"
                    yield Vehicle(
                        accident_id=int(row["Num_Acc"]),
                        vehicle_id=(
                            int(str(row["id_vehicule"]).replace(" ", ""))
                            if "id_vehicule" in row else None
                        ),
                        vehicle_name=str(row["num_veh"]),
                        traffic_direction=(
                            TrafficDirection(int(row["senc"]))
                            if row["senc"] not in {"", "0", "-1"} else None
                        ),
                        vehicle_category=(
                            VehicleCategory(int(row["catv"]))
                            if row["catv"] not in {"0", "-1"} else None
                        ),
                        fixed_obstacle=(
                            FixedObstacle(int(row["obs"]))
                            if (
                                    row["obs"] not in {"", "00", "0", "-1"}
                            ) else None
                        ),
                        mobile_obstacle=(
                            MobileObstacle(int(row["obsm"]))
                            if row["obsm"] not in {"", "0", "-1"} else None
                        ),
                        shock_point=(
                            ShockPoint(int(row["choc"]))
                            if row["choc"] not in {"", "0", "-1"} else None
                        ),
                        primary_manoeuvre=(
                            Manoeuvre(int(row["manv"]))
                            if (
                                    row["manv"] not in {"", "00", "0", "-1"}
                            ) else None
                        ),
                        engine=(
                            Engine(int(row["motor"]))
                            if (
                                    "motor" in row and
                                    row["motor"] not in {"0", "-1"}
                            ) else None
                        ),
                        occupancy=(
                            int(row["occutc"])
                            if row["occutc"] != "" else None
                        ),
                    )
                    progress.update(1)
                except:
                    print(path, row)
                    raise

    def parse(self, input_paths: list[Path]) -> Iterable[Vehicle]:
        progress = tqdm(
            desc="Parsing vehicles",
            total=count_lines(input_paths) - len(input_paths),
            unit="line",
        )
        for path in input_paths:
            yield from self._parse_file(path, progress)
