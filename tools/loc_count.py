#!/usr/bin/env python3
"""Report Haskell and Python LOC percentages."""

from __future__ import annotations

import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def count_lang(path: Path, extensions: tuple[str, ...]) -> int:
    total = 0
    for ext in extensions:
        for file in path.rglob(f"*{ext}"):
            if file.is_file():
                total += sum(1 for _ in file.read_text(encoding="utf-8", errors="ignore").splitlines())
    return total


def main() -> None:
    haskell_loc = count_lang(ROOT / "haskell", (".hs",))
    python_loc = count_lang(ROOT / "python" / "src", (".py",))
    total = haskell_loc + python_loc
    report = {
        "haskell_loc": haskell_loc,
        "python_loc": python_loc,
        "total_loc": total,
        "haskell_share": round(haskell_loc / total * 100, 2) if total else 0,
        "python_share": round(python_loc / total * 100, 2) if total else 0,
    }
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
