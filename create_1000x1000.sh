#!/usr/bin/env bash
set -euo pipefail

# Run full pipeline for a 1000x1000 map (seed=1000)
ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$ROOT_DIR/python"

uv run python -m fleet.cli run \
  --seed 1000 \
  --width 1000 \
  --height 1000 \
  --no-fly 0 \
  --depots 12 \
  --battery 5000000 \
  --obstacle-density 0.02 \
  --coverage-algorithm stc \
  --max-drones 24 \
  --mp4
