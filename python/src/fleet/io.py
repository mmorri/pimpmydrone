"""Utility helpers for reading and writing project JSON files."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any


def read_json(path: Path) -> Any:
    """Load JSON from *path* using UTF-8 encoding."""
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def write_json(path: Path, data: Any) -> None:
    """Serialize *data* to *path* creating parent directories as needed."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as handle:
        json.dump(data, handle, indent=2, sort_keys=True)


def ensure_directory(path: Path) -> Path:
    """Create *path* directory if it is missing and return it."""
    path.mkdir(parents=True, exist_ok=True)
    return path
