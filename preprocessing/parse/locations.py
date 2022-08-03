from csv import DictReader
from pathlib import Path
from typing import Iterable

from tqdm.auto import tqdm

from model import (
    Location, Curvature, Profile, DedicatedLane, TrafficRegime, RoadCategory
)
from parse import Parser
from parse.util import count_lines


class LocationsCsvParser(Parser[Location]):

    @staticmethod
    def _parse_file(
            path: Path,
            progress: tqdm,
    ) -> Iterable[Location]:
        file_year = int(path.name.split("-")[-1].removesuffix(".csv"))
        delimiter: str
        if file_year >= 2019:
            delimiter = ";"
        else:
            delimiter = ","
        with path.open("r", encoding="latin-1") as file:
            reader = DictReader(file, delimiter=delimiter, quotechar='"')
            for row in reader:
                for key in row:
                    row[key] = str(row[key]).strip()
                if "." in row["pr"]:
                    row["pr"], row["pr1"] = row["pr1"], row["pr"]
                if row["Num_Acc"] == "200500068514":
                    assert row["catr"] == ""
                    row["catr"] = "9"
                yield Location(
                    accident_id=int(row["Num_Acc"]),
                    road_category=RoadCategory(int(row["catr"])),
                    road=str(row["voie"]),
                    road_index_number=(
                        int(row["v1"])
                        if row["v1"] != "" else None
                    ),
                    road_index_alpha=(
                        str(row["v2"])
                        if row["v2"] != "N/A" else None
                    ),
                    traffic_regime=(
                        TrafficRegime(int(row["circ"]))
                        if row["circ"] not in {"", "0", "-1"} else None
                    ),
                    lanes_count=(
                        int(row["nbv"])
                        if row["nbv"] != "" else None
                    ),
                    dedicated_lane=(
                        DedicatedLane(int(row["vosp"]))
                        if row["vosp"] not in {"", "-1"} else None
                    ),
                    profile=(
                        Profile(int(row["prof"]))
                        if row["prof"] not in {"", "0", "-1"} else None
                    ),
                    upstream_terminal=(
                        int(
                            str(row["pr"])
                            .removeprefix("(")
                            .removesuffix(")")
                        )
                        if row["pr"] not in {"", "-1"} else None
                    ),
                    upstream_terminal_distance_meters=(
                        float(
                            str(row["pr1"])
                            .removeprefix("(")
                            .removesuffix(")")
                        )
                        if row["pr1"] not in {"", "-1"} else None
                    ),
                    curvature=(
                        Curvature(int(row["plan"]))
                        if row["plan"] not in {"", "0", "-1"} else None
                    ),
                    central_reservation_width_meters=(
                        float(
                            str(row["lartpc"]).replace(",", ".")
                        )
                        if row["lartpc"] != "" else 0
                    ),
                    road_traffic_width_meters=(
                        float(
                            str(row["larrout"]).replace(",", ".")
                        )
                        if row["larrout"] != "" else 0
                    ),
                )
                progress.update(1)

    def parse(self, input_paths: list[Path]) -> Iterable[Location]:
        progress = tqdm(
            desc="Parsing locations",
            total=count_lines(input_paths),
            unit="line",
        )
        for path in input_paths:
            yield from self._parse_file(path, progress)
