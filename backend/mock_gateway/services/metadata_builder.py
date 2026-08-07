from pathlib import Path

LOCAL_METADATA_PATH = Path(__file__).resolve().parents[3] / "app" / "localService" / "metadata.xml"


def build_metadata() -> str:
    return LOCAL_METADATA_PATH.read_text(encoding="utf-8")
