from pathlib import Path

from parse import Parser


class VehicleParser(Parser):
    def parse(self, input_paths: list[Path], output_dir: Path) -> None:
        raise NotImplementedError()
