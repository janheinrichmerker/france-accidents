from datetime import datetime
from enum import IntEnum
from json import JSONEncoder, dumps
from pathlib import Path
from typing import Iterable, Any

from tqdm.auto import tqdm

from model import Accident, Person, Vehicle
from parse import Formatter


def _convert(o):
    if isinstance(o, datetime):
        return o.isoformat()
    elif isinstance(o, dict):
        return {
            k: _convert(v)
            for k, v in o.items()
        }
    elif isinstance(o, list):
        return [_convert(v) for v in o]
    elif isinstance(o, set):
        return _convert(list(o))
    elif isinstance(o, IntEnum):
        return o.name
    elif isinstance(o, (Accident, Vehicle, Person)):
        return _convert(o._asdict())
    else:
        return o


class _Encoder(JSONEncoder):
    def default(self, o: Any) -> Any:
        if isinstance(o, datetime):
            return o.isoformat()
        elif isinstance(o, set):
            return tuple(o)
        elif isinstance(o, IntEnum):
            return o.name
        elif isinstance(o, (Accident, Vehicle, Person)):
            return o._asdict()
        else:
            return super().default(o)


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
                item = _convert(item)
                file.write(f"{dumps(item)}\n")


def _person_json(person: Person) -> dict:
    json = person._asdict()
    json["safety_equipment"] = list(json["safety_equipment"])
    return json
