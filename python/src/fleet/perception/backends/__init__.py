"""Pluggable perception model backends."""

from __future__ import annotations

import os
from typing import Any

from .base import PerceptionBackend, PerceptionInput, list_backend_status
from .demo import DemoBackend
from .domain import DomainBackend
from .onnx_cnn import OnnxCnnBackend
from .vlm_http import VlmHttpBackend
from .yolo import YoloBackend

_REGISTRY: dict[str, PerceptionBackend] = {
    DemoBackend.name: DemoBackend(),
    YoloBackend.name: YoloBackend(),
    OnnxCnnBackend.name: OnnxCnnBackend(),
    VlmHttpBackend.name: VlmHttpBackend(),
    DomainBackend.name: DomainBackend(),
}


def get_backend(name: str) -> PerceptionBackend:
    key = name.strip().lower()
    if key not in _REGISTRY:
        raise KeyError(f"Unknown perception backend {name!r}")
    return _REGISTRY[key]


def available_backends() -> list[dict[str, Any]]:
    enabled = {
        part.strip().lower()
        for part in os.environ.get("FLEET_PERCEPTION_BACKENDS", "demo").split(",")
        if part.strip()
    }
    # Always allow demo; others need to be listed OR auto if installed.
    status = []
    for name, backend in _REGISTRY.items():
        info = backend.status()
        installed = bool(info.get("installed"))
        explicitly = name in enabled or enabled == {"all"}
        usable = installed and (explicitly or name == "demo" or info.get("auto"))
        status.append(
            {
                "name": name,
                "installed": installed,
                "enabled": usable,
                "detail": info.get("detail", ""),
            }
        )
    return status


def run_detect(name: str, inputs: PerceptionInput) -> dict[str, Any]:
    backend = get_backend(name)
    st = backend.status()
    if not st.get("installed"):
        raise RuntimeError(
            f"Backend {name!r} is not installed: {st.get('detail', 'missing dependency')}"
        )
    return backend.detect(inputs)


__all__ = [
    "PerceptionBackend",
    "PerceptionInput",
    "available_backends",
    "get_backend",
    "list_backend_status",
    "run_detect",
]
