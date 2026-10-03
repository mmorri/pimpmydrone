"""Import-path / entry-point domain model plugins."""

from __future__ import annotations

import importlib
import os
from typing import Any

from .base import PerceptionInput


class DomainBackend:
    name = "domain"

    def status(self) -> dict[str, Any]:
        target = os.environ.get("FLEET_DOMAIN_PLUGIN", "")
        return {
            "installed": bool(target),
            "auto": False,
            "detail": (
                f"Plugin {target}"
                if target
                else "Set FLEET_DOMAIN_PLUGIN=package.module:Factory"
            ),
        }

    def detect(self, inputs: PerceptionInput) -> dict[str, Any]:
        target = os.environ.get("FLEET_DOMAIN_PLUGIN", "")
        if not target:
            raise RuntimeError(
                "Domain backend needs FLEET_DOMAIN_PLUGIN=module.path:callable"
            )
        plugin = _load_plugin(target)
        result = plugin(inputs) if callable(plugin) else plugin.detect(inputs)
        if not isinstance(result, dict) or result.get("type") != "FeatureCollection":
            raise RuntimeError("Domain plugin must return a FeatureCollection dict")
        # Stamp source if missing.
        for feature in result.get("features", []):
            props = feature.setdefault("properties", {})
            props.setdefault("source", "domain")
        return result


def _load_plugin(target: str) -> Any:
    if ":" not in target:
        raise RuntimeError("FLEET_DOMAIN_PLUGIN must look like package.module:attr")
    module_name, attr = target.split(":", 1)
    module = importlib.import_module(module_name)
    obj: Any = module
    for part in attr.split("."):
        obj = getattr(obj, part)
    if callable(obj):
        try:
            inst = obj()
            if hasattr(inst, "detect"):
                return inst
        except TypeError:
            pass
        return obj
    return obj


__all__ = ["DomainBackend"]
