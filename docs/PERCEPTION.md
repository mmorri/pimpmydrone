# Perception + ML integration

PimpMyDrone is the **mission brain**. YOLO, lightweight ONNX CNNs, VLMs/SLMs,
and domain models plug in as **perception backends** that emit one shared
GeoJSON schema. The planner turns `cover` / `avoid` / `inspect` / `depot`
detections into disclaimer-gated QGC exports.

This is **not** sense-and-avoid. Detections are untrusted inputs.

## Schema

See [`schema/perception.schema.json`](../schema/perception.schema.json).

Every feature needs:

| property | values |
| --- | --- |
| `class` | model label (`solar_panel`, `person`, …) |
| `confidence` | 0–1 |
| `role` | `cover` \| `avoid` \| `inspect` \| `depot` \| `track` |
| `source` | `yolo` \| `cnn` \| `vlm` \| `domain` \| `manual` \| `demo` |

Geometry: `Polygon` / `MultiPolygon` / `Point` (points are buffered).

## Ontologies

`solar`, `security`, `ag`, `thermal`, `wildlife` — class→role tables in
`fleet.perception.ontology`. Override with request `role_map` or per-feature
`role`.

## API

| endpoint | purpose |
| --- | --- |
| `GET /api/perception/backends` | installed / enabled backends |
| `GET /api/perception/ontology` | class→role tables |
| `POST /api/perception/ingest` | validate + compile → site GeoJSON |
| `POST /api/perception/detect` | run a named backend |
| `POST /api/perception/command` | NL command (+ optional image) |
| `POST /api/plan/from-perception` | compile + full plan job |

All mutating routes require `accepted_disclaimer: true`.

## CLI

```bash
uv run python -m fleet perception backends
uv run python -m fleet perception detect --backend demo --output /tmp/det.json
uv run python -m fleet perception ingest --input examples/perception/solar_detections.geojson \
  --output /tmp/site.geojson
uv run python -m fleet perception plan-from-perception \
  --input examples/perception/solar_detections.geojson \
  --output-dir ../outputs/perc-solar --vertical solar
```

## Backends

| name | dependency | env |
| --- | --- | --- |
| `demo` | none | always on |
| `yolo` | `uv sync --extra yolo` | `FLEET_YOLO_MODEL` (default `yolov8n.pt`) |
| `onnx` | `uv sync --extra onnx` | `FLEET_ONNX_MODEL`, optional `FLEET_ONNX_LABELS` |
| `vlm` | `httpx` (core) | `FLEET_VLM_URL` OpenAI-compatible base, `FLEET_VLM_MODEL` |
| `domain` | your code | `FLEET_DOMAIN_PLUGIN=package.module:Factory` |

Enable non-demo backends explicitly:

```bash
export FLEET_PERCEPTION_BACKENDS=demo,yolo,onnx,vlm,domain
```

### YOLO (Ultralytics)

```bash
uv sync --extra yolo
export FLEET_YOLO_MODEL=yolov8n-seg.pt
# Provide frame_georef or center_lon/center_lat when calling /api/perception/detect
```

### ONNX CNN

Export a tiny classifier to ONNX. The stock adapter expects `extras.tensor`
(NCHW float32) or a full preprocess pipeline via a **domain plugin**.

### VLM / SLM (llama.cpp, vLLM, Ollama)

```bash
export FLEET_VLM_URL=http://127.0.0.1:8080/v1
export FLEET_VLM_MODEL=llava
```

Without `FLEET_VLM_URL`, the VLM backend falls back to the demo NL keyword parser.

### Domain plugin

```python
# my_thermal.py
def Factory():
    class Plugin:
        def detect(self, inputs):
            return {"type": "FeatureCollection", "features": [...]}
    return Plugin
```

```bash
export FLEET_DOMAIN_PLUGIN=my_thermal:Factory
```

## Hangar UI

`#perception` on the public site: demo detections, NL command, upload, then
**Plan from perception** (same liability gate as the planner).
