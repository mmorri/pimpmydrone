# Product & funding

PimpMyDrone is **free experimental software**. There is **no subscription** and
**no paid warranty**. Development is funded by **donations**.

## Before anyone flies anything

1. Giant liability gate on the website (`/`) — must accept to enter.
2. Every API plan requires `accepted_disclaimer: true`.
3. Every mission bundle includes `SAFETY_REPORT.md` + `DISCLAIMER.txt`.
4. Full legal text: [`LEGAL.md`](../LEGAL.md) and `/legal`.

**Authors accept no responsibility for crash, injury, death, property damage,
airspace violations, or any other loss.**

## Run the public surface

```bash
make build
make serve          # http://127.0.0.1:8000
# Optional: pull OSM buildings as keep-outs before planning
uv run python -m fleet from-geo --geojson ../examples/sites/solar_industrial.geojson \
  --output-dir ../outputs/site-industrial --vertical solar --osm-buildings
```

Optional env:

| Variable | Purpose |
| --- | --- |
| `FLEET_DONATE_URL` | Primary donation link (default GitHub Sponsors) |
| `FLEET_DONATE_ALT_URL` | Secondary tip jar (default Ko-fi) |
| `FLEET_GITHUB_URL` | Repo link |
| `FLEET_MAX_CELLS` | Fair-use grid cap for the demo (not a paywall) |
| `FLEET_RATE_LIMIT_PER_MIN` | Plan POST rate limit per IP (default 12) |
| `FLEET_CORS_ORIGINS` | Comma-separated origins (default `*`) |
| `FLEET_JOBS` | Temp job directory |

Enable [GitHub Sponsors](https://github.com/sponsors/mmorri) and/or
[Ko-fi](https://ko-fi.com/pimpmydrone). Donations never buy a warranty.

## CLI

```bash
make site
uv run python -m fleet from-geo --geojson ../examples/sites/solar_farm.geojson \
  --output-dir ../outputs/site-solar --vertical solar
# Compare region vs strip partitions and keep the better schedule:
uv run python -m fleet from-geo --geojson ../examples/sites/solar_farm.geojson \
  --output-dir ../outputs/site-solar-auto --vertical solar \
  --partition auto --schedule-mode columns
# Same knobs in the hangar: Schedule mode + partition auto (default).
```

## Perception / ML adapters

YOLO, ONNX CNNs, VLMs, and domain plugins feed a shared detection schema.
See [`docs/PERCEPTION.md`](PERCEPTION.md). Hangar UI: `#perception`.

CV/VLM outputs are **untrusted** and are not sense-and-avoid.

## Operator guide

Public case-study README: `/guide` (also `/readme`) — solar polygon path,
perception path, export/preflight, and CLI twin.

## What “good enough to ask for donations” means here

- Planner + simulator still verify coverage.
- Exports are clearly labelled drafts with a preflight checklist.
- Website refuses to plan without disclaimer acceptance.
- Funding ask is gratitude, not access control.
