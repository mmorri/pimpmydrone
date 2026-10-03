"""Optional Ultralytics YOLO (v8/v11) backend."""

from __future__ import annotations

import base64
import os
from typing import Any

from ..ontology import resolve_role
from .base import PerceptionInput, pixel_to_lonlat


class YoloBackend:
    name = "yolo"

    def status(self) -> dict[str, Any]:
        try:
            import ultralytics  # noqa: F401

            return {
                "installed": True,
                "auto": False,
                "detail": f"ultralytics available; model={os.environ.get('FLEET_YOLO_MODEL', 'yolov8n.pt')}",
            }
        except ImportError:
            return {
                "installed": False,
                "auto": False,
                "detail": "pip/uv install ultralytics (optional extra: yolo)",
            }

    def detect(self, inputs: PerceptionInput) -> dict[str, Any]:
        try:
            from ultralytics import YOLO
        except ImportError as exc:  # pragma: no cover
            raise RuntimeError(
                "YOLO backend requires ultralytics. Install with: "
                "uv sync --extra yolo"
            ) from exc

        model_name = os.environ.get("FLEET_YOLO_MODEL", "yolov8n.pt")
        model = YOLO(model_name)
        image = _resolve_image(inputs)
        results = model.predict(image, verbose=False)
        features: list[dict[str, Any]] = []
        ontology = inputs.ontology or "solar"
        for result in results:
            names = result.names or {}
            h, w = (
                (result.orig_shape[0], result.orig_shape[1])
                if result.orig_shape
                else (480, 640)
            )
            boxes = getattr(result, "boxes", None)
            if boxes is None:
                continue
            for box in boxes:
                cls_id = int(box.cls.item()) if box.cls is not None else 0
                conf = float(box.conf.item()) if box.conf is not None else 0.0
                label = str(names.get(cls_id, cls_id))
                xyxy = box.xyxy[0].tolist()
                x1, y1, x2, y2 = xyxy
                corners = [
                    (x1, y1),
                    (x2, y1),
                    (x2, y2),
                    (x1, y2),
                    (x1, y1),
                ]
                ring = [
                    list(
                        pixel_to_lonlat(
                            x,
                            y,
                            frame_georef=inputs.frame_georef,
                            center_lonlat=inputs.center_lonlat,
                            width=w,
                            height=h,
                        )
                    )
                    for x, y in corners
                ]
                meta = resolve_role(label, ontology=ontology)
                features.append(
                    {
                        "type": "Feature",
                        "geometry": {"type": "Polygon", "coordinates": [ring]},
                        "properties": {
                            "class": label,
                            "confidence": conf,
                            "role": meta["role"],
                            "source": "yolo",
                            "model": model_name,
                            **{
                                k: v
                                for k, v in meta.items()
                                if k != "role" and k not in {"class"}
                            },
                        },
                    }
                )
        return {"type": "FeatureCollection", "features": features}


def _resolve_image(inputs: PerceptionInput) -> Any:
    if inputs.image_path:
        return str(inputs.image_path)
    if inputs.image_bytes:
        return inputs.image_bytes
    if inputs.image_b64:
        return base64.b64decode(inputs.image_b64)
    if inputs.image_url:
        import urllib.request

        with urllib.request.urlopen(inputs.image_url, timeout=30) as resp:  # noqa: S310
            return resp.read()
    raise ValueError("YOLO backend needs image_path, image_bytes, image_b64, or image_url")


__all__ = ["YoloBackend"]
