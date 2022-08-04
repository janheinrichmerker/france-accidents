from json import dumps
from pathlib import Path
from typing import Iterable

from tqdm.auto import tqdm

from model import Accident
from parse import Formatter


class AccidentsJsonlFormatter(Formatter[Accident]):
    def format(
            self,
            items: Iterable[Accident],
            output_dir: Path
    ) -> None:
        output_path = output_dir / "accidents.jsonl"
        items = tqdm(
            items,
            desc="Formatting accidents",
            unit="line",
        )
        with output_path.open("w") as file:
            for item in items:
                json = item._asdict()
                json["timestamp"] = json["timestamp"].isoformat()
                json["vehicles"] = [
                    vehicle._asdict()
                    for vehicle in json["vehicles"]
                ]
                json["persons"] = [
                    person._asdict()
                    for person in json["persons"]
                ]
                file.write(f"{dumps(json)}\n")
