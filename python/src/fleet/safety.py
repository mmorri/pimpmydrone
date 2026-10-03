"""Preflight / liability artefacts shipped with every mission bundle."""

from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from . import io

DISCLAIMER_SHORT = (
    "PimpMyDrone is experimental planning software only. It does NOT replace a "
    "licensed remote pilot, airspace authorization, manufacturer manuals, or "
    "on-site safety judgment. You alone are responsible for any flight."
)

DISCLAIMER_FULL = """\
# LIABILITY DISCLAIMER — READ BEFORE YOU FLY

**PimpMyDrone / Fleet coverage planner is provided AS IS, with NO WARRANTY of any
kind — express or implied — including merchantability, fitness for a particular
purpose, non-infringement, accuracy, completeness, or uninterrupted availability.**

By downloading, generating, importing, or flying any plan produced by this
software, **you acknowledge and agree that:**

1. **You are solely responsible** for the aircraft, payload, crew, bystanders,
   property, livestock, wildlife, and airspace. The authors, contributors,
   hosts, and donors accept **no liability** for crash, injury, death, regulatory
   violation, privacy breach, data loss, opportunity cost, or any other damage —
   whether arising in contract, tort, or otherwise — even if advised of the
   possibility.
2. Generated paths are **approximate discrete-grid tours**. They do **not** model
   wind, terrain elevation, buildings, power lines, trees, birds, other aircraft,
   RF interference, GPS denial, battery degradation, magnetic declination,
   geofences, NOTAMs, TFRs, controlled airspace, or local law.
3. Exports (QGroundControl `.plan`, CSV, GeoJSON) are **drafts for review**. You
   must validate every waypoint in your ground station, confirm home/RTH, altitude
   reference (AGL vs AMSL), fail-safes, and collision avoidance **before arming**.
4. Multi-drone concurrency is scheduled for makespan only; **spatial deconfliction
   is NOT guaranteed**. Flying multiple vehicles from these plans without your own
   separation protocol is unsafe.
5. Coverage percentages refer to **raster cells on a synthetic grid**, not sensor
   footprint, GSD, spray deposition, or regulatory “inspection complete” criteria.
6. Use is at your own risk. If you do not accept this disclaimer in full, **do not
   use the software and do not fly the plans**.

This notice is also embedded in every mission bundle as `SAFETY_REPORT.md`.
"""


def build_safety_report(
    *,
    metrics: dict[str, Any] | None,
    geo: dict[str, Any] | None,
    manifest: dict[str, Any] | None,
    vertical: str,
    perception: dict[str, Any] | None = None,
) -> str:
    """Markdown report that must travel with every downloadable mission."""

    lines = [
        "# Mission safety report",
        "",
        f"Generated (UTC): {datetime.now(timezone.utc).isoformat()}",
        f"Vertical preset: `{vertical}`",
        "",
        "## Hard disclaimer",
        "",
        DISCLAIMER_FULL.strip(),
        "",
        "## Preflight checklist (operator must complete)",
        "",
        "- [ ] I am legally allowed to fly here (airspace, waivers, privacy, insurance).",
        "- [ ] I loaded each `.plan` into QGroundControl / my GCS and inspected the map.",
        "- [ ] Home / RTH point, geofence, and max altitude match this site.",
        "- [ ] Altitude mode (relative vs AMSL) matches my vehicle configuration.",
        "- [ ] Battery, wind, weather, and daylight are within my aircraft limits.",
        "- [ ] I have a lost-link and low-battery procedure; I am ready to take manual control.",
        "- [ ] If using more than one aircraft, I have a separation / deconfliction plan.",
        "- [ ] If perception / CV / VLM detections shaped this plan, I verified every keep-out and target on the map.",
        "- [ ] I accept that this tool authors accept **no responsibility** for the flight.",
        "",
    ]

    warnings = collect_warnings(metrics=metrics, geo=geo, perception=perception)
    lines.append("## Automated warnings from this run")
    lines.append("")
    if warnings:
        for warning in warnings:
            lines.append(f"- **WARNING:** {warning}")
    else:
        lines.append("- No automated warnings beyond the hard disclaimer above.")
    lines.append("")

    if perception:
        counts = perception.get("counts") or {}
        lines.extend(
            [
                "## Perception (experimental — not sense-and-avoid)",
                "",
                perception.get(
                    "disclaimer",
                    "Computer vision / VLM outputs are untrusted inputs.",
                ),
                "",
                f"- Ontology: `{perception.get('ontology', '?')}`",
                f"- Sources: {', '.join(perception.get('sources') or []) or 'n/a'}",
                f"- Cover detections: {counts.get('cover', 0)}",
                f"- Avoid / keep-out: {counts.get('avoid', 0)}",
                f"- Inspect targets: {counts.get('inspect', 0)}",
                f"- Depots from perception: {counts.get('depot', 0)}",
                "",
                "False positives and false negatives are expected. This stack is "
                "**not** a certified sense-and-avoid, detect-and-avoid, or "
                "people-counting product.",
                "",
            ]
        )

    if metrics:
        cov = metrics.get("coverage", {})
        lines.extend(
            [
                "## Planner metrics (not a flight clearance)",
                "",
                f"- Coverage of reachable grid cells: **{cov.get('percentage', '?')}%**",
                f"- Reachable cells: {cov.get('reachable_cells', '?')}",
                f"- Unreachable / sealed pockets: {cov.get('unreachable_cells', '?')}",
                f"- Out of battery range (skipped): {cov.get('out_of_range_cells', cov.get('skipped_cells', '?'))}",
                f"- Drones used: {metrics.get('drones', {}).get('used', '?')}",
                f"- Makespan (simulated): {metrics.get('makespan', '?')} s",
                f"- Total path length (grid steps): {metrics.get('total_length', '?')}",
                "",
            ]
        )
        problems = (metrics.get("checks") or {}).get("problems") or []
        if problems:
            lines.append("## Simulator problems")
            lines.append("")
            for problem in problems:
                lines.append(f"- {problem}")
            lines.append("")

    if geo:
        lines.extend(
            [
                "## Georeferencing used for export",
                "",
                f"- Origin lon/lat: `{geo.get('origin_lon')}`, `{geo.get('origin_lat')}`",
                f"- Cell size: {geo.get('cell_size_m')} m",
                f"- Grid: {geo.get('width')} × {geo.get('height')}",
                f"- Export altitude (relative draft): {geo.get('altitude_m')} m",
                "",
                "Cell centres are projected with a local equirectangular approximation. "
                "Expect metre-level error that grows with site size — survey-grade "
                "accuracy is **not** claimed.",
                "",
            ]
        )

    if manifest:
        lines.extend(
            [
                "## Files in this bundle",
                "",
                f"- Missions GeoJSON: `{Path(str(manifest.get('geojson', ''))).name}`",
                f"- Waypoints CSV: `{Path(str(manifest.get('csv', ''))).name}`",
                f"- QGC plans: {len(manifest.get('qgc_plans') or [])}",
                f"- Path simplification: {manifest.get('simplified')}",
                "",
            ]
        )

    lines.extend(
        [
            "## Support the project (donations, not subscriptions)",
            "",
            "PimpMyDrone stays free to use. If it helped your work, fund development "
            "with a donation — there is no paid tier and no warranty upgrade.",
            "",
        ]
    )
    return "\n".join(lines) + "\n"


def collect_warnings(
    *,
    metrics: dict[str, Any] | None,
    geo: dict[str, Any] | None,
    perception: dict[str, Any] | None = None,
) -> list[str]:
    warnings: list[str] = []
    if perception:
        warnings.append(
            "Mission geometry was influenced by perception detections "
            "(YOLO/CNN/VLM/domain) — verify cover/avoid/inspect on a real map before arming."
        )
    if not metrics:
        warnings.append("No simulator metrics were attached to this export.")
        return warnings

    cov = metrics.get("coverage") or {}
    pct = float(cov.get("percentage") or 0.0)
    if pct < 99.999:
        warnings.append(
            f"Coverage is {pct:.2f}% of reachable cells — gaps remain; do not treat as complete."
        )

    problems = (metrics.get("checks") or {}).get("problems") or []
    if problems:
        warnings.append(
            f"Simulator reported {len(problems)} problem(s); do not fly until resolved."
        )

    drones = (metrics.get("drones") or {}).get("used") or 0
    if drones > 1:
        warnings.append(
            "Multiple drones were scheduled without spatial deconfliction — "
            "collision risk is yours to manage."
        )

    efficiency = metrics.get("efficiency") or {}
    commute = efficiency.get("commute_share")
    if commute is not None and float(commute) > 0.7:
        warnings.append(
            f"Commute share is {float(commute):.0%} of flight — "
            "battery and traffic risk are dominated by depot trips."
        )

    if geo and float(geo.get("cell_size_m") or 0) >= 20:
        warnings.append(
            "Coarse cell size (≥ 20 m) — paths are sketch-level and may miss narrow corridors."
        )

    return warnings


def write_safety_bundle(
    out_dir: Path,
    *,
    metrics: dict[str, Any] | None,
    geo: dict[str, Any] | None,
    manifest: dict[str, Any] | None,
    vertical: str,
    perception: dict[str, Any] | None = None,
) -> Path:
    io.ensure_directory(out_dir)
    report = build_safety_report(
        metrics=metrics,
        geo=geo,
        manifest=manifest,
        vertical=vertical,
        perception=perception,
    )
    path = out_dir / "SAFETY_REPORT.md"
    path.write_text(report, encoding="utf-8")
    (out_dir / "DISCLAIMER.txt").write_text(DISCLAIMER_FULL, encoding="utf-8")
    return path


__all__ = [
    "DISCLAIMER_FULL",
    "DISCLAIMER_SHORT",
    "build_safety_report",
    "collect_warnings",
    "write_safety_bundle",
]
