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
```

Optional env:

| Variable | Purpose |
| --- | --- |
| `FLEET_DONATE_URL` | Donation link (default GitHub Sponsors) |
| `FLEET_GITHUB_URL` | Repo link |
| `FLEET_MAX_CELLS` | Fair-use grid cap for the demo (not a paywall) |
| `FLEET_JOBS` | Temp job directory |

Enable [GitHub Sponsors](https://github.com/sponsors/mmorri) (or point
`FLEET_DONATE_URL` at Ko-fi / PayPal / Open Collective).

## CLI

```bash
make site
uv run python -m fleet from-geo --geojson ../examples/sites/solar_farm.geojson \
  --output-dir ../outputs/site-solar --vertical solar
```

## Perception / ML adapters

YOLO, ONNX CNNs, VLMs, and domain plugins feed a shared detection schema.
See [`docs/PERCEPTION.md`](PERCEPTION.md). Hangar UI: `#perception`.

CV/VLM outputs are **untrusted** and are not sense-and-avoid.

## What “good enough to ask for donations” means here

- Planner + simulator still verify coverage.
- Exports are clearly labelled drafts with a preflight checklist.
- Website refuses to plan without disclaimer acceptance.
- Funding ask is gratitude, not access control.
