from json import loads
from pathlib import Path

from typing import NamedTuple, Optional

from bs4 import BeautifulSoup
from requests import get
from tqdm.auto import tqdm

# Directory paths.
PROJECT_DIR = Path(__file__).parent.parent
DATA_DIR = PROJECT_DIR / "static" / "data"
DATA_DIR.mkdir(exist_ok=True)
CACHE_DIR = DATA_DIR / "cache"
CACHE_DIR.mkdir(exist_ok=True)

# URLs
DATASET_URL = "https://www.data.gouv.fr/en/datasets/53698f4ca3a729239d2036df/"


class Artifact(NamedTuple):
    name: str
    url: str


def parse_artifact(json: dict) -> Optional[Artifact]:
    if "fileFormat" not in json:
        return None
    if json["fileFormat"] != "text/csv":
        return None
    if not str(json["name"]).endswith(".csv"):
        return None
    return Artifact(json["name"], json["contentUrl"])


def get_artifacts() -> list[Artifact]:
    response = get(DATASET_URL)
    document = BeautifulSoup(response.text, 'html.parser')
    json_string = document.find(id="json_ld").string
    json = loads(json_string)
    print(f"License information at {json['license']}")
    artifacts = [
        parse_artifact(artifact)
        for artifact in json["distribution"]
    ]
    artifacts = [
        artifact
        for artifact in artifacts
        if artifact is not None
    ]
    assert len({
        artifact.name
        for artifact in artifacts
    }) == len(artifacts)
    return artifacts


def cache_artifact(artifact: Artifact) -> None:
    path = CACHE_DIR / artifact.name
    if path.exists():
        assert path.is_file()
        return
    response = get(artifact.url)
    with path.open("wb") as file:
        file.write(response.content)


def cache_artifacts() -> None:
    artifacts = get_artifacts()
    artifacts = tqdm(
        artifacts,
        desc="Downloading artifacts",
        unit="file"
    )
    for artifact in artifacts:
        cache_artifact(artifact)


def main() -> None:
    cache_artifacts()


if __name__ == '__main__':
    main()
