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

# Dataset base URL to fetch available data.
DATASET_URL = "https://data.gouv.fr/en/datasets/53698f4ca3a729239d2036df/"


class _Artifact(NamedTuple):
    name: str
    url: str


def _parse_artifact(json: dict) -> Optional[_Artifact]:
    if "fileFormat" not in json:
        return None
    if json["fileFormat"] != "text/csv":
        return None
    if not str(json["name"]).endswith(".csv"):
        return None
    name: str = json["name"]
    name = name.replace("_", "-")
    return _Artifact(name, json["contentUrl"])


def _get_artifacts() -> list[_Artifact]:
    response = get(DATASET_URL)
    document = BeautifulSoup(response.text, 'html.parser')
    json_string = document.find(id="json_ld").string
    json = loads(json_string)
    print(f"License information at {json['license']}")
    artifacts = [
        _parse_artifact(artifact)
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


def _cache_artifact(artifact: _Artifact) -> Path:
    path = CACHE_DIR / artifact.name
    if path.exists():
        assert path.is_file()
        return path
    response = get(artifact.url)
    with path.open("wb") as file:
        file.write(response.content)
    return path


def cache_artifacts() -> list[Path]:
    artifacts = _get_artifacts()
    artifacts = tqdm(
        artifacts,
        desc="Downloading artifacts",
        unit="file"
    )
    return [
        _cache_artifact(artifact)
        for artifact in artifacts
    ]
