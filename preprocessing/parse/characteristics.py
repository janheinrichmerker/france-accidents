from csv import DictReader
from datetime import datetime
from pathlib import Path
from typing import Iterable

from tqdm.auto import tqdm

from model import (
    Characteristic, Light, Intersection, AtmosphericConditions, Collision,
    LocationRegime
)
from parse import Parser
from parse.util import count_lines


class CharacteristicsCsvParser(Parser[Characteristic]):
    @staticmethod
    def _parse_file(
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

    def parse(self, input_paths: list[Path]) -> Iterable[Characteristic]:
        progress = tqdm(
            desc="Parsing characteristics",
            total=count_lines(input_paths),
            unit="line",
        )
        for path in input_paths:
            yield from self._parse_file(path, progress)
