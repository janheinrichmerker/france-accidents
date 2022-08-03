from abc import ABC, abstractmethod
from pathlib import Path
from typing import Generic, TypeVar, Iterable

T = TypeVar("T")


class Parser(ABC, Generic[T]):
    @abstractmethod
    def parse(self, input_paths: list[Path]) -> Iterable[T]:
        pass


class Formatter(ABC, Generic[T]):
    @abstractmethod
    def format(self, items: Iterable[T], output_dir: Path) -> None:
        pass
