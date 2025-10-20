#!/usr/bin/env bash
set -euo pipefail

# Run full pipeline for a 100x100 map (seed=100)
ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$ROOT_DIR/python"

uv run python -m fleet.cli run \
  --seed 100 \
  --width 100 \
  --height 100 \
  --no-fly 0 \
  --depots 3 \
  --battery 10000 \
  --obstacle-density 0.1 \
  --coverage-algorithm stc \
  --max-drones 5 \
  --mp4
