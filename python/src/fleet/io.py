"""Utility helpers for reading and writing project JSON files."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

# Payloads above this size (in compact bytes) are written without indentation:
# the pure-Python pretty printer is several times slower than the C encoder,
# which matters for million-waypoint tour files.
PRETTY_LIMIT = 1_000_000


def read_json(path: Path) -> Any:
    """Load JSON from *path* using UTF-8 encoding."""
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def write_json(path: Path, data: Any) -> None:
    """Serialize *data* to *path* creating parent directories as needed.

    Small files are pretty-printed for readability; large ones are compact.
    """
    path.parent.mkdir(parents=True, exist_ok=True)
    text = json.dumps(data, sort_keys=True)
    if len(text) <= PRETTY_LIMIT:
        text = json.dumps(data, indent=2, sort_keys=True)
    with path.open("w", encoding="utf-8") as handle:
        handle.write(text)
        handle.write("\n")


def ensure_directory(path: Path) -> Path:
    """Create *path* directory if it is missing and return it."""
    path.mkdir(parents=True, exist_ok=True)
    return path
