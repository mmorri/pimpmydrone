# LEGAL DISCLAIMER

**Last updated:** 2026-10-02

## Experimental software — fly at your own risk

PimpMyDrone (also referred to as the Fleet coverage planner) is **experimental
research / hobby software**. It produces **draft** multi-drone coverage paths on
a discrete grid and optional ground-station exports.

**THE SOFTWARE AND ALL PLANS, MAPS, METRICS, AND EXPORTS ARE PROVIDED “AS IS”
AND “AS AVAILABLE”, WITHOUT WARRANTY OF ANY KIND.**

## No responsibility for damage

To the maximum extent permitted by law, the authors, copyright holders,
contributors, hosts, mirror operators, and donors:

- **accept no responsibility and no liability** for any direct, indirect,
  incidental, special, consequential, exemplary, or punitive damages;
- **accept no responsibility** for crash, collision, fire, injury, death,
  property damage, livestock or wildlife harm, environmental harm, privacy
  violation, regulatory fine, license suspension, data loss, or business
  interruption;
- **accept no responsibility** for airspace infringement, NOTAM/TFR violations,
  export-control issues, or misuse of generated missions;

whether caused by software defect, modelling error, user error, third-party
tools (including QGroundControl / autopilot firmware), or otherwise — **even if
advised of the possibility of such damage**.

If applicable law does not allow a full exclusion of liability, liability is
limited to the greater of (a) zero / the amount you donated for the copy you
used, or (b) the minimum amount required by that law.

## Not a substitute for a remote pilot

You must:

1. hold any licences, waivers, and insurance required where you fly;
2. check airspace, weather, NOTAMs, and local rules yourself;
3. inspect every waypoint in your own ground control station before arming;
4. maintain visual line of sight / approved BVLOS procedures as required;
5. never treat coverage % or “simulation OK” as a flight clearance;
6. treat any computer-vision, YOLO, CNN, thermal, or VLM/SLM detections that
   shaped a plan as **untrusted suggestions** — not sense-and-avoid, not
   people-counting certification, and not a substitute for your own survey.

## Perception / ML inputs

Optional perception adapters accept detections from third-party models
(Ultralytics YOLO, ONNX CNNs, llama.cpp / vLLM / Ollama VLMs, domain plugins).
Those models, weights, prompts, and georeferencing transforms are outside this
project’s control. **False positives and false negatives are expected.** Authors
accept **no liability** for missions planned from bad detections, missed
obstacles, misclassified people or wildlife, or hallucinated VLM geometry.

## No paid warranty

Donations fund development time. **A donation does not purchase support,
indemnity, insurance, certification, or any warranty.** There is no
subscription and no service-level agreement.

## Acceptance

Using the website, API, CLI, or any exported mission constitutes acceptance of
this disclaimer. If you do not agree, do not use the software and do not fly
the plans.

See also `SAFETY_REPORT.md` embedded in every mission download.
