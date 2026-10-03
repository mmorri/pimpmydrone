"""Optional ONNX Runtime lightweight CNN backend for obstacle / hotspot cues."""

from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Any

from ..ontology import resolve_role
from .base import PerceptionInput, pixel_to_lonlat


class OnnxCnnBackend:
    name = "onnx"

    def status(self) -> dict[str, Any]:
        model = os.environ.get("FLEET_ONNX_MODEL", "")
        try:
            import onnxruntime  # noqa: F401

            installed = True
            detail = f"onnxruntime available; model={model or '(set FLEET_ONNX_MODEL)'}"
        except ImportError:
            installed = False
            detail = "pip/uv install onnxruntime (optional extra: onnx)"
        return {
            "installed": installed,
            "auto": False,
            "detail": detail if installed else detail,
        }

    def detect(self, inputs: PerceptionInput) -> dict[str, Any]:
        try:
            import numpy as np
            import onnxruntime as ort
        except ImportError as exc:  # pragma: no cover
            raise RuntimeError(
                "ONNX backend requires onnxruntime. Install with: uv sync --extra onnx"
            ) from exc

        model_path = os.environ.get("FLEET_ONNX_MODEL")
        if not model_path or not Path(model_path).exists():
            raise RuntimeError("Set FLEET_ONNX_MODEL to an ONNX file path")

        labels_path = os.environ.get("FLEET_ONNX_LABELS")
        labels = _load_labels(labels_path)
        session = ort.InferenceSession(model_path, providers=["CPUExecutionProvider"])
        # Minimal contract: model input image NCHW float32, output class scores.
        # Callers with custom graphs should wrap via the domain plugin instead.
        if not inputs.image_path and not inputs.image_bytes and not inputs.image_b64:
            raise ValueError("ONNX backend needs an image input")

        # Without forcing heavyweight image codecs into the core path, accept a
        # pre-baked tensor in extras or fall back to a centre-point classification.
        if "tensor" in inputs.extras:
            tensor = np.asarray(inputs.extras["tensor"], dtype=np.float32)
        else:
            raise RuntimeError(
                "ONNX demo path expects extras.tensor (NCHW float32). "
                "Use a domain plugin for full decode+preprocess pipelines."
            )

        input_name = session.get_inputs()[0].name
        outputs = session.run(None, {input_name: tensor})
        scores = np.asarray(outputs[0]).reshape(-1)
        cls_id = int(scores.argmax())
        conf = float(scores[cls_id]) if scores.size else 0.0
        # Softmax-ish if scores look like logits.
        if scores.max() > 1.0 or scores.min() < 0:
            exp = np.exp(scores - scores.max())
            conf = float(exp[cls_id] / exp.sum())
        label = labels[cls_id] if cls_id < len(labels) else f"class_{cls_id}"
        ontology = inputs.ontology or "solar"
        meta = resolve_role(label, ontology=ontology)
        lon, lat = inputs.center_lonlat or (-122.2515, 37.7735)
        if inputs.frame_georef:
            lon, lat = pixel_to_lonlat(
                0.5,
                0.5,
                frame_georef=inputs.frame_georef,
                center_lonlat=inputs.center_lonlat,
                width=1,
                height=1,
            )
        return {
            "type": "FeatureCollection",
            "features": [
                {
                    "type": "Feature",
                    "geometry": {"type": "Point", "coordinates": [lon, lat]},
                    "properties": {
                        "class": label,
                        "confidence": conf,
                        "role": meta["role"],
                        "source": "cnn",
                        "model": Path(model_path).name,
                        **{k: v for k, v in meta.items() if k != "role"},
                    },
                }
            ],
        }


def _load_labels(path: str | None) -> list[str]:
    if not path or not Path(path).exists():
        return ["obstacle", "clear", "hotspot"]
    text = Path(path).read_text(encoding="utf-8")
    if path.endswith(".json"):
        data = json.loads(text)
        if isinstance(data, list):
            return [str(x) for x in data]
        if isinstance(data, dict):
            return [str(data[k]) for k in sorted(data, key=lambda x: int(x))]
    return [line.strip() for line in text.splitlines() if line.strip()]


__all__ = ["OnnxCnnBackend"]
