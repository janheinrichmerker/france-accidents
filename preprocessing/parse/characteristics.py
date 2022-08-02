from csv import DictReader, DictWriter
from datetime import datetime
from pathlib import Path
from typing import Iterable

from tqdm.auto import tqdm

from parse import Parser
from model import Characteristic, Light, Intersection, AtmosphericConditions, \
    Collision, LocationRegime


class CharacteristicsParser(Parser):

    def _parse_file(
            self,
            path: Path,
            progress: tqdm,
    ) -> Iterable[Characteristic]:
        file_year = int(path.name.split("-")[-1].removesuffix(".csv"))
        delimiter: str
        if file_year >= 2019:
            delimiter = ";"
        elif file_year == 2009:
            delimiter = "\t"
        else:
            delimiter = ","
        with path.open("r", encoding="latin-1") as file:
            reader = DictReader(file, delimiter=delimiter, quotechar='"')
            for row in reader:
                hour_minute_str = str(row["hrmn"]).replace(":", "")
                assert len(hour_minute_str) <= 4
                hour_minute_str = (
                    f"{'0' * (4 - len(hour_minute_str))}{hour_minute_str}"
                )
                assert len(hour_minute_str) == 4

                yield Characteristic(
                    accident_id=int(row["Num_Acc"]),
                    timestamp=datetime(
                        year=int(row["an"]),
                        month=int(row["mois"]),
                        day=int(row["jour"]),
                        hour=int(hour_minute_str[0:2]),
                        minute=int(hour_minute_str[2:4]),
                    ),
                    latitude=(
                        float(row["lat"].replace(",", "."))
                        if row["lat"] not in ["", "-"] else None
                    ),
                    longitude=(
                        float(row["long"].replace(",", "."))
                        if row["long"] not in ["", "-"] else None
                    ),
                    address=str(row["adr"]),
                    light=(
                        Light(int(row["lum"]))
                        if int(row["lum"]) != -1
                        else None
                    ),
                    intersection=(
                        Intersection(int(row["int"]))
                        if int(row["int"]) != -1 and int(row["int"]) != 0
                        else None
                    ),
                    atmospheric_conditions=(
                        AtmosphericConditions(int(row["atm"]))
                        if row["atm"] != "" and int(row["atm"]) != -1
                        else None
                    ),
                    collision=(
                        Collision(int(row["col"]))
                        if row["col"] != "" and int(row["col"]) != -1
                        else None
                    ),
                    location=(
                        LocationRegime(int(row["agg"]))
                        if True or row["agg"] != ""
                        else None
                    ),
                    department=str(row["dep"]),
                    commune=str(row["com"]),
                )
                progress.update(1)

    def parse(self, input_paths: list[Path], output_dir: Path) -> None:
        output_path = output_dir / "characteristics.csv"

        # Compute total count.
        num_lines = 0
        for path in input_paths:
            with path.open("r", encoding="latin-1") as file:
                num_lines += sum(1 for _ in file)

        progress = tqdm(
            desc="Parsing characteristics",
            total=num_lines,
            unit="line",
        )

        with output_path.open("w") as output_file:
            fieldnames = [
                "accident_id",
                "timestamp",
                "latitude",
                "longitude",
                "department",
                "commune",
                "address",
                "location",
                "light",
                "atmospheric_conditions",
                "intersection",
                "collision",
            ]
            writer = DictWriter(output_file, fieldnames=fieldnames)
            writer.writeheader()
            for path in input_paths:
                for characteristic in self._parse_file(path, progress):
                    writer.writerow({
                        "accident_id": characteristic.accident_id,
                        "timestamp": characteristic.timestamp.isoformat(),
                        "latitude": characteristic.latitude or "",
                        "longitude": characteristic.longitude or "",
                        "department": characteristic.department,
                        "commune": characteristic.commune,
                        "address": characteristic.address,
                        "location": (
                            characteristic.location.name
                            if characteristic.location else ""
                        ),
                        "light": (
                            characteristic.light.name
                            if characteristic.light else ""
                        ),
                        "atmospheric_conditions": (
                            characteristic.atmospheric_conditions.name
                            if characteristic.atmospheric_conditions else ""
                        ),
                        "intersection": (
                            characteristic.intersection.name
                            if characteristic.intersection else ""
                        ),
                        "collision": (
                            characteristic.collision.name
                            if characteristic.collision else ""
                        ),
                    })
