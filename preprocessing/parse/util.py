from pathlib import Path
from typing import Iterable


def count_lines(file_paths: Iterable[Path]) -> int:
    num_lines = 0
    for path in file_paths:
        with path.open("r", encoding="latin-1") as file:
            num_lines += sum(1 for _ in file)
    return num_lines
