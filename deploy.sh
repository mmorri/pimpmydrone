#!/usr/bin/env bash
# Deploy PimpMyDrone web UI to a remote host (Docker on port 8080).
set -euo pipefail

HOST="${DEPLOY_HOST:-167.17.68.61}"
USER="${DEPLOY_USER:-mmorri}"
REMOTE_DIR="${DEPLOY_DIR:-/home/${USER}/pimpmydrone}"
ROOT="$(cd "$(dirname "$0")" && pwd)"

if [[ ! -x "$ROOT/haskell/bin/coverage-planner" || ! -x "$ROOT/haskell/bin/coverage-partitioner" ]]; then
  echo "Missing haskell binaries. Run: make build" >&2
  exit 1
fi

echo "→ syncing to ${USER}@${HOST}:${REMOTE_DIR}"
rsync -az --delete \
  --exclude '.git/' \
  --exclude 'python/.venv/' \
  --exclude '**/__pycache__/' \
  --exclude 'outputs/' \
  --exclude 'haskell/.stack-work/' \
  --exclude '.uv-cache/' \
  "$ROOT/" "${USER}@${HOST}:${REMOTE_DIR}/"

echo "→ building & starting container"
ssh "${USER}@${HOST}" "cd '${REMOTE_DIR}' && docker compose up -d --build"

echo "→ health check"
ok=0
for _ in 1 2 3 4 5 6 7 8; do
  if ssh "${USER}@${HOST}" "curl -fsS --max-time 3 http://127.0.0.1:8080/api/health"; then
    echo
    ok=1
    break
  fi
  sleep 2
done
if [[ "$ok" -ne 1 ]]; then
  echo "Health check failed after container start" >&2
  exit 1
fi

echo "Live at: http://${HOST}:8080/"
echo "Disclaimer gate is on the homepage. Donations: /donate"
