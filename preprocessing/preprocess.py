from pathlib import Path

from cache import cache_artifacts
from parse.characteristics import CharacteristicsCsvParser


def _matching_files(files: list[Path], prefix: str) -> list[Path]:
    return [
        file
        for file in files
        if file.name.startswith(prefix)
    ]


def main() -> None:
    files: list[Path] = cache_artifacts()
    characteristics = CharacteristicsCsvParser().parse(
        _matching_files(files, "caracteristiques")
    )
    print(sum(1 for _ in characteristics))


if __name__ == "__main__":
    main()
