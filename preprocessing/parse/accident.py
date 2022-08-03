from json import dumps
from pathlib import Path
from typing import Iterable

from model import (
    Characteristic, Accident
)
from parse import Formatter


class AccidentsJsonlFormatter(Formatter[Accident]):
    def format(
            self,
            items: Iterable[Characteristic],
            output_dir: Path
    ) -> None:
        output_path = output_dir / "accidents.jsonl"

        with output_path.open("w") as file:
            for item in items:
                json = {
                    "accident_id": item.accident_id,
                    "timestamp": item.timestamp.isoformat(),
                    "latitude": item.latitude or "",
                    "longitude": item.longitude or "",
                    "department": item.department,
                    "commune": item.commune,
                    "address": item.address,
                    "location": (
                        item.location.name
                        if item.location else ""
                    ),
                    "light": (
                        item.light.name
                        if item.light else ""
                    ),
                    "atmospheric_conditions": (
                        item.atmospheric_conditions.name
                        if item.atmospheric_conditions else ""
                    ),
                    "intersection": (
                        item.intersection.name
                        if item.intersection else ""
                    ),
                    "collision": (
                        item.collision.name
                        if item.collision else ""
                    ),
                }
                file.write(f"{dumps(json)}\n")
