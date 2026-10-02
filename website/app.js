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
  const fileInput = document.getElementById("file");
  fileInput.addEventListener("change", async (ev) => {
    const file = ev.target.files[0];
    if (!file) return;
    uploadedGeo = JSON.parse(await file.text());
    drawn.clearLayers();
    const layer = L.geoJSON(uploadedGeo);
    drawn.addLayer(layer);
    map.fitBounds(layer.getBounds(), { padding: [24, 24] });
  });

  document.getElementById("clearBtn").onclick = () => {
    drawn.clearLayers();
    uploadedGeo = null;
    fileInput.value = "";
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
  loadVerticals();

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
      accepted_disclaimer: true,
    };
    const maxDrones = document.getElementById("maxDrones").value;
    if (maxDrones) body.max_drones = Number(maxDrones);

    const btn = document.getElementById("planBtn");
    btn.disabled = true;
    btn.textContent = "Planning…";
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
      btn.textContent = "Plan coverage";
    }
  };

  function showResult(data) {
    const result = document.getElementById("result");
    result.hidden = false;
    document.getElementById("metrics").innerHTML = `
      <div class="metric"><span>Coverage</span><b>${Number(data.coverage_pct).toFixed(1)}%</b></div>
      <div class="metric"><span>Makespan</span><b>${Number(data.makespan).toFixed(0)} s</b></div>
      <div class="metric"><span>Drones</span><b>${data.drones}</b></div>
      <div class="metric"><span>Path length</span><b>${Number(data.total_length).toFixed(0)}</b></div>
    `;
    const warnings = document.getElementById("warnings");
    const list = data.warnings || [];
    warnings.innerHTML = list.length
      ? `<strong>Before you fly</strong><ul>${list.map((w) => `<li>${w}</li>`).join("")}</ul>`
      : "";
    document.getElementById("preview").src = data.png_url + "?t=" + Date.now();
    const qgc = (data.qgc_urls || [])
      .map((u, i) => `<a href="${u}" download>QGC plan — drone ${i}</a>`)
      .join("");
    document.getElementById("downloads").innerHTML = `
      <a href="${data.safety_url}" download>SAFETY_REPORT.md (read this)</a>
      <a href="${data.missions_geojson_url}" download>Missions GeoJSON</a>
      <a href="${data.waypoints_csv_url}" download>Waypoints CSV</a>
      <a href="${data.bundle_url}" download>Full job zip</a>
      ${qgc}
    `;
  }
})();
