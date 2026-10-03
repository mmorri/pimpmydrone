"""OpenAI-compatible VLM/SLM HTTP backend (llama.cpp, vLLM, Ollama)."""

from __future__ import annotations

import json
import os
import re
from typing import Any

from ..ontology import resolve_role
from .base import PerceptionInput
from .demo import DemoBackend


class VlmHttpBackend:
    name = "vlm"

    def status(self) -> dict[str, Any]:
        base = os.environ.get("FLEET_VLM_URL", "").rstrip("/")
        return {
            "installed": True,
            "auto": True,
            "detail": (
                f"OpenAI-compatible endpoint {base}"
                if base
                else "No FLEET_VLM_URL — falls back to demo NL parser"
            ),
        }

    def detect(self, inputs: PerceptionInput) -> dict[str, Any]:
        base = os.environ.get("FLEET_VLM_URL", "").rstrip("/")
        if not base:
            # Graceful demo fallback so the hangar NL box always does something.
            return DemoBackend().detect(inputs)

        try:
            import httpx
        except ImportError as exc:  # pragma: no cover
            raise RuntimeError("VLM backend requires httpx") from exc

        model = os.environ.get("FLEET_VLM_MODEL", "llava")
        api_key = os.environ.get("FLEET_VLM_API_KEY", "no-key")
        ontology = inputs.ontology or "solar"
        lon0, lat0 = inputs.center_lonlat or (-122.2515, 37.7735)
        prompt = _build_prompt(inputs.text or "", ontology, lon0, lat0)
        messages: list[dict[str, Any]] = [{"role": "user", "content": prompt}]
        if inputs.image_b64:
            messages = [
                {
                    "role": "user",
                    "content": [
                        {"type": "text", "text": prompt},
                        {
                            "type": "image_url",
                            "image_url": {
                                "url": f"data:image/jpeg;base64,{inputs.image_b64}"
                            },
                        },
                    ],
                }
            ]
        payload = {
            "model": model,
            "messages": messages,
            "temperature": 0.1,
            "response_format": {"type": "json_object"},
        }
        headers = {"Authorization": f"Bearer {api_key}"}
        with httpx.Client(timeout=60.0) as client:
            resp = client.post(f"{base}/chat/completions", json=payload, headers=headers)
            resp.raise_for_status()
            data = resp.json()
        content = data["choices"][0]["message"]["content"]
        parsed = _parse_json_content(content)
        features = []
        for item in parsed.get("detections", []):
            class_name = str(item.get("class") or "target")
            meta = resolve_role(
                class_name,
                ontology=ontology,
                explicit_role=item.get("role"),
            )
            coords = item.get("coordinates") or [lon0, lat0]
            geom_type = item.get("geometry") or "Point"
            if geom_type == "Polygon" and isinstance(coords[0], (list, tuple)):
                geometry = {"type": "Polygon", "coordinates": [coords]}
            else:
                geometry = {"type": "Point", "coordinates": coords[:2]}
            features.append(
                {
                    "type": "Feature",
                    "geometry": geometry,
                    "properties": {
                        "class": class_name,
                        "confidence": float(item.get("confidence") or 0.6),
                        "role": meta["role"],
                        "source": "vlm",
                        "model": model,
                        **{k: v for k, v in meta.items() if k != "role"},
                    },
                }
            )
        if not features:
            return DemoBackend().detect(inputs)
        return {"type": "FeatureCollection", "features": features}


def _build_prompt(text: str, ontology: str, lon0: float, lat0: float) -> str:
    return (
        "You are a drone perception planner. Reply with ONLY JSON of the form "
        '{"detections":[{"class":"...","role":"cover|avoid|inspect|depot",'
        '"confidence":0.0,"coordinates":[lon,lat],"geometry":"Point"}]}. '
        f"Ontology hint: {ontology}. Scene centre lon/lat: {lon0}, {lat0}. "
        f"Operator command: {text or 'detect relevant objects for a coverage mission.'}"
    )


def _parse_json_content(content: str) -> dict[str, Any]:
    content = content.strip()
    try:
        return json.loads(content)
    except json.JSONDecodeError:
        match = re.search(r"\{.*\}", content, flags=re.S)
        if not match:
            return {"detections": []}
        try:
            return json.loads(match.group(0))
        except json.JSONDecodeError:
            return {"detections": []}


__all__ = ["VlmHttpBackend"]
