#!/usr/bin/env bash
set -euo pipefail

# Run full pipeline for a 10x10 map (seed=10)
ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$ROOT_DIR/python"

uv run python -m fleet.cli run \
  --seed 10 \
  --width 10 \
  --height 10 \
  --no-fly 0 \
  --depots 1 \
  --obstacle-density 0.1 \
  --coverage-algorithm stc \
  --max-drones 5 \
  --mp4
