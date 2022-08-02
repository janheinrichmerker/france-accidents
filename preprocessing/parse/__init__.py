from abc import ABC, abstractmethod
from pathlib import Path


class Parser(ABC):
    @abstractmethod
    def parse(self, input_paths: list[Path], output_dir: Path) -> None:
        pass

