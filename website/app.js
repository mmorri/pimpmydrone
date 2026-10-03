(() => {
  const cfg = window.FLEET_CONFIG || {};
  const donateUrl = cfg.donate_url || "/donate";
  const githubUrl = cfg.github_url || "https://github.com/mmorri/pimpmydrone";

  const gate = document.getElementById("gate");
  const gateAccept = document.getElementById("gateAccept");
  const gateEnter = document.getElementById("gateEnter");
  const planAccept = document.getElementById("planAccept");

  if (cfg.disclaimer_short) {
    const hero = document.getElementById("heroDisclaimer");
    if (hero) hero.textContent = cfg.disclaimer_short;
  }
  const maxCells = document.getElementById("maxCells");
  if (maxCells && cfg.max_cells) maxCells.textContent = String(cfg.max_cells);

  ["navGithub"].forEach((id) => {
    const el = document.getElementById(id);
    if (el) el.href = githubUrl;
  });
  ["gateDonate", "resultDonate"].forEach((id) => {
    const el = document.getElementById(id);
    if (el && el.tagName === "A") el.href = donateUrl.startsWith("http") ? donateUrl : "/donate";
  });

  const unlocked = sessionStorage.getItem("pmd_disclaimer_ok") === "1";
  if (unlocked && gate) gate.hidden = true;

  if (gateAccept && gateEnter) {
    gateAccept.addEventListener("change", () => {
      gateEnter.disabled = !gateAccept.checked;
    });
    gateEnter.addEventListener("click", () => {
      if (!gateAccept.checked) return;
      sessionStorage.setItem("pmd_disclaimer_ok", "1");
      if (planAccept) planAccept.checked = true;
      gate.hidden = true;
    });
  }

  if (!document.getElementById("map")) return;

  const map = L.map("map").setView([37.7735, -122.2515], 14);
  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    attribution: "&copy; OpenStreetMap",
  }).addTo(map);

  const drawn = new L.FeatureGroup();
  map.addLayer(drawn);
  map.addControl(
    new L.Control.Draw({
      draw: {
        polygon: { allowIntersection: false, showArea: true },
        rectangle: true,
        polyline: false,
        circle: false,
        marker: false,
        circlemarker: false,
      },
      edit: { featureGroup: drawn },
    })
  );
  map.on(L.Draw.Event.CREATED, (e) => {
    drawn.clearLayers();
    drawn.addLayer(e.layer);
  });

  let uploadedGeo = null;
  let activeSiteId = "";
  const fileInput = document.getElementById("file");
  fileInput.addEventListener("change", async (ev) => {
    const file = ev.target.files[0];
    if (!file) return;
    uploadedGeo = JSON.parse(await file.text());
    activeSiteId = "";
    drawn.clearLayers();
    const layer = L.geoJSON(uploadedGeo);
    drawn.addLayer(layer);
    map.fitBounds(layer.getBounds(), { padding: [24, 24] });
  });

  document.getElementById("clearBtn").onclick = () => {
    drawn.clearLayers();
    uploadedGeo = null;
    activeSiteId = "";
    fileInput.value = "";
    const example = document.getElementById("exampleSite");
    if (example) example.value = "";
  };

  async function loadVerticals() {
    const res = await fetch("/api/verticals");
    const data = await res.json();
    const select = document.getElementById("vertical");
    for (const [name, preset] of Object.entries(data)) {
      const opt = document.createElement("option");
      opt.value = name;
      opt.textContent = `${name} — ${preset.description}`;
      if (name === "solar") opt.selected = true;
      select.appendChild(opt);
    }
  }

  async function loadExamples() {
    const select = document.getElementById("exampleSite");
    const demos = document.getElementById("demoSites");
    const btn = document.getElementById("loadExampleBtn");
    if (!select) return;
    try {
      const res = await fetch("/api/examples");
      const data = await res.json();
      for (const site of data.sites || []) {
        const opt = document.createElement("option");
        opt.value = site.id;
        opt.textContent = `${site.title} — ${site.description}`;
        opt.dataset.vertical = site.vertical || "";
        (demos || select).appendChild(opt);
      }
    } catch (_err) {
      /* hangar still works by drawing / upload */
    }
    const applyGeo = (geo, id, vertical) => {
      uploadedGeo = geo;
      activeSiteId = id || "";
      drawn.clearLayers();
      const layer = L.geoJSON(geo);
      drawn.addLayer(layer);
      map.fitBounds(layer.getBounds(), { padding: [24, 24] });
      fileInput.value = "";
      const vertSel = document.getElementById("vertical");
      if (vertical && vertSel && [...vertSel.options].some((o) => o.value === vertical)) {
        vertSel.value = vertical;
      }
    };
    const applyExample = async () => {
      const id = select.value;
      if (!id) return;
      const error = document.getElementById("error");
      try {
        const res = await fetch(`/api/examples/${encodeURIComponent(id)}`);
        const geo = await res.json();
        if (!res.ok) throw new Error(geo.detail || "Could not load example");
        const vertical = select.selectedOptions[0]?.dataset.vertical;
        applyGeo(geo, id, vertical);
        if (error) error.hidden = true;
      } catch (err) {
        if (error) {
          error.hidden = false;
          error.textContent = String(err.message || err);
        }
      }
    };
    select.addEventListener("change", applyExample);
    if (btn) btn.addEventListener("click", applyExample);
    const fieldId = new URLSearchParams(location.search).get("field") || "";
    if (/^dwr_[0-9]{4}_[0-9]+$/.test(fieldId)) {
      try {
        const res = await fetch(`/api/fields/${encodeURIComponent(fieldId)}`);
        const geo = await res.json();
        if (!res.ok) throw new Error(geo.detail || "Could not load field");
        const cover = (geo.features || []).find((feature) => feature.properties) || {};
        applyGeo(geo, fieldId, (cover.properties || {}).vertical || "generic");
      } catch (err) {
        const error = document.getElementById("error");
        if (error) {
          error.hidden = false;
          error.textContent = String(err.message || err);
        }
      }
    }
  }
  loadVerticals().then(loadExamples);

  function currentGeoJSON() {
    if (uploadedGeo) return uploadedGeo;
    const layers = drawn.getLayers();
    if (!layers.length) return null;
    return layers[0].toGeoJSON();
  }

  document.getElementById("planBtn").onclick = async () => {
    const error = document.getElementById("error");
    const result = document.getElementById("result");
    error.hidden = true;
    result.hidden = true;
    error.textContent = "";

    if (!planAccept.checked) {
      error.hidden = false;
      error.textContent =
        "Re-confirm the liability disclaimer before planning. Authors accept no responsibility for flights.";
      return;
    }
    const geojson = currentGeoJSON();
    if (!geojson) {
      error.hidden = false;
      error.textContent = "Draw or upload a site polygon first.";
      return;
    }

    const body = {
      geojson,
      vertical: document.getElementById("vertical").value,
      schedule_mode: document.getElementById("scheduleMode")?.value || "sortie",
      osm_buildings: Boolean(document.getElementById("osmBuildings")?.checked),
      accepted_disclaimer: true,
    };
    const maxDrones = document.getElementById("maxDrones").value;
    if (maxDrones) body.max_drones = Number(maxDrones);
    const altitudeM = document.getElementById("altitudeM")?.value;
    if (altitudeM) body.altitude_m = Number(altitudeM);

    const btn = document.getElementById("planBtn");
    const status = document.getElementById("planStatus");
    btn.disabled = true;
    btn.textContent = "Customizing…";
    if (status) {
      status.hidden = false;
      status.textContent = "Partitioning the hangar floor and battery-tuning tours…";
    }
    try {
      const res = await fetch("/api/plan", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body),
      });
      const data = await res.json();
      if (!res.ok) {
        const detail = data.detail || JSON.stringify(data);
        throw new Error(typeof detail === "string" ? detail : JSON.stringify(detail));
      }
      showResult(data);
    } catch (err) {
      error.hidden = false;
      error.textContent = String(err.message || err);
    } finally {
      btn.disabled = false;
      btn.textContent = "Customize mission";
      if (status) status.hidden = true;
    }
  };

  function showResult(data) {
    const result = document.getElementById("result");
    result.hidden = false;
    const insp =
      data.inspection_pct == null
        ? ""
        : `<div class="metric"><span>Inspection (approx)</span><b>${Number(
            data.inspection_pct
          ).toFixed(1)}%</b></div>`;
    const scores = data.partition_scores || {};
    const scoreHint = Object.keys(scores).length
      ? ` <small>(${Object.entries(scores)
          .map(([k, v]) => `${k} ${Number(v).toFixed(0)}`)
          .join(" / ")})</small>`
      : "";
    const partition = data.partition_family
      ? `<div class="metric"><span>Partition</span><b>${data.partition_family}${scoreHint}</b></div>`
      : "";
    const schedule = data.schedule_mode
      ? `<div class="metric"><span>Schedule</span><b>${data.schedule_mode}</b></div>`
      : "";
    const osm =
      data.osm_buildings == null
        ? ""
        : `<div class="metric"><span>OSM keep-outs</span><b>${
            data.osm_ok === false ? "skipped" : data.osm_buildings
          }</b></div>`;
    const deconf =
      data.deconflicted == null
        ? ""
        : `<div class="metric"><span>Sim deconflict</span><b>${
            data.deconflicted ? "ok" : "conflicts"
          }</b></div>`;
    document.getElementById("metrics").innerHTML = `
      <div class="metric"><span>Coverage</span><b>${Number(data.coverage_pct).toFixed(1)}%</b></div>
      ${insp}
      <div class="metric"><span>Makespan</span><b>${Number(data.makespan).toFixed(0)} s</b></div>
      <div class="metric"><span>Drones</span><b>${data.drones}</b></div>
      <div class="metric"><span>Path length</span><b>${Number(data.total_length).toFixed(0)}</b></div>
      ${partition}
      ${schedule}
      ${osm}
      ${deconf}
    `;
    const warnings = document.getElementById("warnings");
    const list = data.warnings || [];
    warnings.innerHTML = list.length
      ? `<strong>Before you fly</strong><ul>${list.map((w) => `<li>${w}</li>`).join("")}</ul>`
      : "";
    document.getElementById("preview").src = data.png_url + "?t=" + Date.now();
    const extras = [];
    if (data.coverage_age_url) {
      extras.push(
        `<a href="${data.coverage_age_url}" target="_blank" rel="noopener">Coverage-age heatmap</a>`
      );
    }
    if (data.sortie_gantt_url) {
      extras.push(
        `<a href="${data.sortie_gantt_url}" target="_blank" rel="noopener">Sortie Gantt</a>`
      );
    }
    if (data.osm_url) {
      extras.push(`<a href="${data.osm_url}" download>OSM buildings report</a>`);
    }
    const select = document.getElementById("exampleSite");
    const fromSelect =
      select && /^[a-z0-9_]+$/.test(select.value || "") ? select.value : "";
    const slug = /^[a-z0-9_]+$/.test(activeSiteId) ? activeSiteId : fromSelect || "site";
    const qgc = (data.qgc_urls || [])
      .map(
        (u, i) =>
          `<a href="${u}" download="${slug}-drone-${i}.plan">QGC plan — drone ${i}</a>`
      )
      .join("");
    document.getElementById("downloads").innerHTML = `
      <a href="${data.safety_url}" download="${slug}-SAFETY_REPORT.md">SAFETY_REPORT.md (read this)</a>
      <a href="${data.missions_geojson_url}" download="${slug}-missions.geojson">Missions GeoJSON</a>
      <a href="${data.waypoints_csv_url}" download="${slug}-waypoints.csv">Waypoints CSV</a>
      <a href="${data.bundle_url}" download="${slug}-job.zip">Full job zip</a>
      ${extras.join("")}
      ${qgc}
    `;
  }

  // --- Perception Lab -------------------------------------------------
  const percMapEl = document.getElementById("percMap");
  let perceptionFC = null;
  let percLayer = null;
  let percMap = null;
  if (percMapEl) {
    percMap = L.map("percMap").setView([37.7735, -122.2515], 16);
    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      maxZoom: 19,
      attribution: "&copy; OpenStreetMap",
    }).addTo(percMap);
  }

  const roleColor = {
    cover: "#3dff9a",
    avoid: "#ff5a3d",
    inspect: "#f0c14b",
    depot: "#c44dff",
    track: "#f0c14b",
  };

  function paintPerception(fc, opts) {
    const asPlanInput = !opts || opts.asPlanInput !== false;
    if (asPlanInput) perceptionFC = fc;
    if (!percMap) return;
    if (percLayer) percMap.removeLayer(percLayer);
    percLayer = L.geoJSON(fc, {
      style(feature) {
        const role = (feature.properties && feature.properties.role) || "inspect";
        return {
          color: roleColor[role] || "#e8e4dc",
          weight: 2,
          fillOpacity: role === "avoid" ? 0.35 : 0.2,
        };
      },
      pointToLayer(feature, latlng) {
        const role = (feature.properties && feature.properties.role) || "inspect";
        return L.circleMarker(latlng, {
          radius: 7,
          color: roleColor[role] || "#e8e4dc",
          fillColor: roleColor[role] || "#e8e4dc",
          fillOpacity: 0.85,
        });
      },
      onEachFeature(feature, layer) {
        const p = feature.properties || {};
        layer.bindPopup(
          `<strong>${p.class || "?"}</strong><br/>role=${p.role} · conf=${p.confidence}`
        );
      },
    }).addTo(percMap);
    try {
      percMap.fitBounds(percLayer.getBounds(), { padding: [24, 24] });
    } catch (_) {
      /* empty */
    }
  }

  function showPercReport(report) {
    const el = document.getElementById("percReport");
    if (!el) return;
    el.hidden = false;
    el.textContent = JSON.stringify(report, null, 2);
  }

  async function loadDemoPerception() {
    const ontology = document.getElementById("percOntology").value;
    const res = await fetch("/api/perception/detect", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        backend: "demo",
        ontology,
        accepted_disclaimer: true,
        center_lon: -122.2515,
        center_lat: 37.7735,
      }),
    });
    const data = await res.json();
    if (!res.ok) throw new Error(data.detail || "demo detect failed");
    paintPerception(data.perception);
    const ingest = await fetch("/api/perception/ingest", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        perception: data.perception,
        ontology,
        accepted_disclaimer: true,
      }),
    });
    const compiled = await ingest.json();
    if (ingest.ok) {
      showPercReport(compiled.report);
      // Overlay compiled site geometry but keep raw detections for planning.
      if (compiled.site_geojson) paintPerception(compiled.site_geojson, { asPlanInput: false });
    }
  }

  const demoBtn = document.getElementById("percDemoBtn");
  if (demoBtn) {
    demoBtn.onclick = async () => {
      const error = document.getElementById("error");
      try {
        await loadDemoPerception();
      } catch (err) {
        error.hidden = false;
        error.textContent = String(err.message || err);
      }
    };
  }

  const cmdBtn = document.getElementById("percCommandBtn");
  if (cmdBtn) {
    cmdBtn.onclick = async () => {
      const error = document.getElementById("error");
      const command = document.getElementById("percCommand").value.trim();
      if (!command) {
        error.hidden = false;
        error.textContent = "Enter a natural-language command first.";
        return;
      }
      try {
        const res = await fetch("/api/perception/command", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            command,
            ontology: document.getElementById("percOntology").value,
            backend: "vlm",
            accepted_disclaimer: true,
            center_lon: -122.2515,
            center_lat: 37.7735,
          }),
        });
        const data = await res.json();
        if (!res.ok) throw new Error(data.detail || "command failed");
        paintPerception(data.perception);
      } catch (err) {
        error.hidden = false;
        error.textContent = String(err.message || err);
      }
    };
  }

  const percFile = document.getElementById("percFile");
  if (percFile) {
    percFile.addEventListener("change", async (ev) => {
      const file = ev.target.files[0];
      if (!file) return;
      const fc = JSON.parse(await file.text());
      paintPerception(fc);
    });
  }

  const percPlanBtn = document.getElementById("percPlanBtn");
  if (percPlanBtn) {
    percPlanBtn.onclick = async () => {
      const error = document.getElementById("error");
      const result = document.getElementById("result");
      error.hidden = true;
      if (!planAccept.checked) {
        error.hidden = false;
        error.textContent =
          "Re-confirm the liability disclaimer before planning from perception.";
        return;
      }
      if (!perceptionFC) {
        error.hidden = false;
        error.textContent = "Load demo detections, NL command, or upload GeoJSON first.";
        return;
      }
      const btn = percPlanBtn;
      btn.disabled = true;
      btn.textContent = "Customizing…";
      try {
        const res = await fetch("/api/plan/from-perception", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            perception: perceptionFC,
            ontology: document.getElementById("percOntology").value,
            vertical: document.getElementById("vertical").value || "solar",
            schedule_mode: document.getElementById("scheduleMode")?.value || "sortie",
            osm_buildings: Boolean(document.getElementById("osmBuildings")?.checked),
            accepted_disclaimer: true,
          }),
        });
        const data = await res.json();
        if (!res.ok) {
          const detail = data.detail || JSON.stringify(data);
          throw new Error(typeof detail === "string" ? detail : JSON.stringify(detail));
        }
        if (data.perception_report) showPercReport(data.perception_report);
        showResult(data);
        document.getElementById("planner").scrollIntoView({ behavior: "smooth" });
      } catch (err) {
        error.hidden = false;
        error.textContent = String(err.message || err);
        result.hidden = true;
      } finally {
        btn.disabled = false;
        btn.textContent = "Plan from perception";
      }
    };
  }
})();
