(() => {
  const cfg = window.FLEET_CONFIG || {};
  const gh = document.getElementById("navGithub");
  if (cfg.github_url && gh) gh.href = cfg.github_url;

  const catalogEl = document.getElementById("fieldCatalog");
  const filtersEl = document.getElementById("fieldFilters");
  const sourceEl = document.getElementById("fieldSource");
  const pickEl = document.getElementById("fieldPick");
  const planEl = document.getElementById("fieldPlan");
  const mapEl = document.getElementById("fieldMap");
  if (!catalogEl || !mapEl || typeof L === "undefined") return;

  const CROP_COLOR = {
    Almonds: "#f0c14b",
    Pistachios: "#c44dff",
    Grapes: "#3dff9a",
    Walnuts: "#e8e4dc",
    Corn: "#f0c14b",
    Soybeans: "#3dff9a",
    "Spring wheat": "#e8e4dc",
    "Winter wheat": "#e6c35c",
    "Sugar beets": "#ff4d9a",
    Cotton: "#d7d2c8",
    Rice: "#7ec8ff",
    Peanuts: "#d4a574",
  };

  const map = L.map(mapEl).setView([39.8, -98.6], 4);
  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    attribution: "&copy; OpenStreetMap",
  }).addTo(map);

  const layers = new Map();
  const ranchIds = new Set();
  const ranchMax = Number(cfg.max_ranch_fields) || 4;
  let selected = "";
  let stateFilter = "All";

  function colorFor(crop) {
    return CROP_COLOR[crop] || "#ff4d9a";
  }

  function styleFor(id, crop) {
    const on = id === selected;
    return {
      color: colorFor(crop),
      weight: on ? 4 : 2,
      fillColor: colorFor(crop),
      fillOpacity: on ? 0.45 : 0.25,
    };
  }

  function selectField(id) {
    selected = id;
    layers.forEach((entry, key) => {
      entry.layer.setStyle(styleFor(key, entry.crop));
    });
    document.querySelectorAll(".field-row").forEach((row) => {
      row.classList.toggle("is-selected", row.dataset.id === id);
    });
    const entry = layers.get(id);
    if (!entry) return;
    map.fitBounds(entry.layer.getBounds(), { padding: [28, 28], maxZoom: 16 });
    if (pickEl && planEl) {
      pickEl.hidden = false;
      planEl.href = `/?field=${encodeURIComponent(id)}#planner`;
      planEl.textContent = `Plan ${entry.label}`;
    }
  }

  function styleLabel(style) {
    if (style === "orchard") return "orchard rows";
    if (style === "open-field") return "open-field sweep";
    return "";
  }

  function fieldLabel(field, showCounty) {
    const acres = Number(field.acres);
    const size = Number.isFinite(acres) ? `${acres.toFixed(1)} ac` : field.id;
    if (showCounty && field.county) return `${field.county} · ${size}`;
    return size;
  }

  function addCrop(parent, crop, showCounty) {
    const cropHead = document.createElement("h3");
    const style = styleLabel(crop.flight_style);
    cropHead.textContent = style ? `${crop.crop} · ${style}` : crop.crop;
    cropHead.style.color = colorFor(crop.crop);
    parent.appendChild(cropHead);
    const list = document.createElement("ul");
    for (const field of crop.fields || []) {
      const item = document.createElement("li");
      item.className = "field-line";
      const box = document.createElement("input");
      box.type = "checkbox";
      box.className = "field-check";
      box.checked = ranchIds.has(field.id);
      box.setAttribute("aria-label", `Include ${fieldLabel(field, true)} in the ranch`);
      box.addEventListener("change", () => toggleRanch(field.id, box));
      const button = document.createElement("button");
      button.type = "button";
      button.className = "field-row";
      button.dataset.id = field.id;
      if (field.id === selected) button.classList.add("is-selected");
      button.textContent = fieldLabel(field, showCounty);
      button.addEventListener("click", () => selectField(field.id));
      item.appendChild(box);
      item.appendChild(button);
      list.appendChild(item);
    }
    parent.appendChild(list);
  }

  function renderList(states) {
    catalogEl.replaceChildren();
    for (const state of states) {
      if (stateFilter !== "All" && state.state !== stateFilter) continue;
      const section = document.createElement("section");
      section.className = "field-county";
      const heading = document.createElement("h2");
      heading.textContent = state.state;
      section.appendChild(heading);
      if (state.counties) {
        for (const county of state.counties) {
          const place = document.createElement("p");
          place.className = "field-place";
          place.textContent = county.county;
          section.appendChild(place);
          for (const crop of county.crops || []) addCrop(section, crop, false);
        }
      } else {
        for (const crop of state.crops || []) addCrop(section, crop, true);
      }
      catalogEl.appendChild(section);
    }
  }

  function showState(name) {
    layers.forEach((entry) => {
      const show = name === "All" || entry.state === name;
      if (show) entry.layer.addTo(map);
      else map.removeLayer(entry.layer);
    });
    const visible = [...layers.values()].filter(
      (entry) => name === "All" || entry.state === name
    );
    if (visible.length) {
      const group = L.featureGroup(visible.map((entry) => entry.layer));
      map.fitBounds(group.getBounds(), { padding: [24, 24] });
    }
  }

  function renderFilters(states) {
    if (!filtersEl) return;
    const names = ["All", ...states.map((row) => row.state)];
    filtersEl.replaceChildren();
    for (const name of names) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "field-filter";
      button.textContent = name;
      button.setAttribute("aria-pressed", name === stateFilter ? "true" : "false");
      button.addEventListener("click", () => {
        stateFilter = name;
        filtersEl.querySelectorAll(".field-filter").forEach((el) => {
          el.setAttribute("aria-pressed", el.textContent === name ? "true" : "false");
        });
        renderList(states);
        showState(name);
      });
      filtersEl.appendChild(button);
    }
  }

  function jobsFor(states) {
    const jobs = [];
    for (const state of states) {
      const groups = state.counties
        ? state.counties.flatMap((county) =>
            (county.crops || []).map((crop) => ({ place: county.county, crop }))
          )
        : (state.crops || []).map((crop) => ({ place: "", crop }));
      for (const group of groups) {
        for (const field of group.crop.fields || []) {
          jobs.push({
            state: state.state,
            place: group.place || field.county || "",
            crop: group.crop.crop,
            field,
          });
        }
      }
    }
    return jobs;
  }

  function toggleRanch(id, box) {
    const status = document.getElementById("ranchStatus");
    if (box.checked && ranchIds.size >= ranchMax) {
      box.checked = false;
      if (status) {
        status.textContent = `This hangar plans at most ${ranchMax} fields in one ranch.`;
      }
      return;
    }
    if (box.checked) ranchIds.add(id);
    else ranchIds.delete(id);
    if (status) status.textContent = "";
    syncRanch();
  }

  function syncRanch() {
    const bar = document.getElementById("ranchBar");
    const button = document.getElementById("ranchPlan");
    if (!bar || !button) return;
    const count = ranchIds.size;
    bar.hidden = count === 0;
    button.textContent = count === 1 ? "Plan 1 field" : `Plan ranch (${count})`;
  }

  async function planRanch() {
    const status = document.getElementById("ranchStatus");
    const result = document.getElementById("ranchResult");
    const button = document.getElementById("ranchPlan");
    const accept = document.getElementById("ranchAccept");
    if (!accept || !accept.checked) {
      if (status) status.textContent = "Re-confirm the disclaimer before planning a ranch.";
      return;
    }
    if (button) button.disabled = true;
    if (status) status.textContent = `Planning ${ranchIds.size} field${ranchIds.size === 1 ? "" : "s"}…`;
    if (result) result.hidden = true;
    try {
      const res = await fetch("/api/plan-ranch", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          field_ids: [...ranchIds],
          accepted_disclaimer: true,
          schedule_mode: "sortie",
        }),
      });
      const data = await res.json();
      if (!res.ok) throw new Error(data.detail || "Ranch plan failed");
      const lines = (data.fields || [])
        .map((field) => {
          if (!field.ok) return `<li>${field.folder}: ${field.error || "failed"}</li>`;
          const coverage = Number(field.coverage_pct);
          const makespan = Number(field.makespan);
          const style = styleLabel(field.flight_style);
          const styleBit = style ? ` · ${style}` : "";
          return `<li>${field.folder}${styleBit} — ${coverage.toFixed(0)}% — ${makespan.toFixed(0)} s</li>`;
        })
        .join("");
      if (result) {
        result.hidden = false;
        result.innerHTML = `
          <ul class="ranch-list">${lines}</ul>
          <p><a class="btn" href="${data.bundle_url}" download="${data.bundle_name || "ranch.zip"}">Download ranch</a></p>
        `;
      }
      if (status) status.textContent = data.ok ? "Ranch planned." : "Ranch planned with a field that did not finish.";
    } catch (err) {
      if (status) status.textContent = String(err.message || err);
    } finally {
      if (button) button.disabled = false;
    }
  }

  const ranchButton = document.getElementById("ranchPlan");
  if (ranchButton) ranchButton.addEventListener("click", planRanch);

  async function load() {
    const res = await fetch("/api/fields");
    const catalog = await res.json();
    if (!res.ok) throw new Error(catalog.detail || "Could not load fields");
    if (sourceEl) sourceEl.textContent = catalog.attribution || "";
    const states = catalog.states || [];
    renderFilters(states);
    renderList(states);

    const geos = await Promise.all(
      jobsFor(states).map(async (job) => {
        const geoRes = await fetch(`/api/fields/${encodeURIComponent(job.field.id)}`);
        const geo = await geoRes.json();
        if (!geoRes.ok) return null;
        return { ...job, geo };
      })
    );
    for (const row of geos) {
      if (!row) continue;
      const acres = Number(row.field.acres);
      const label = `${row.place} ${row.state} ${row.crop.toLowerCase()} · ${
        Number.isFinite(acres) ? acres.toFixed(1) : "?"
      } ac`;
      const cover = {
        type: "FeatureCollection",
        features: (row.geo.features || []).filter(
          (feature) => feature.geometry && feature.geometry.type !== "Point"
        ),
      };
      const layer = L.geoJSON(cover, {
        style: () => styleFor(row.field.id, row.crop),
      });
      layer.on("click", () => selectField(row.field.id));
      layer.addTo(map);
      layers.set(row.field.id, {
        layer,
        crop: row.crop,
        state: row.state,
        label,
      });
    }
    if (layers.size) showState("All");
  }

  load().catch((err) => {
    catalogEl.textContent = String(err.message || err);
  });
})();
