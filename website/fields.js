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
    "Sugar beets": "#ff4d9a",
  };

  const map = L.map(mapEl).setView([39.8, -98.6], 4);
  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    attribution: "&copy; OpenStreetMap",
  }).addTo(map);

  const layers = new Map();
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

  function fieldLabel(field, showCounty) {
    const acres = Number(field.acres);
    const size = Number.isFinite(acres) ? `${acres.toFixed(1)} ac` : field.id;
    if (showCounty && field.county) return `${field.county} · ${size}`;
    return size;
  }

  function addCrop(parent, crop, showCounty) {
    const cropHead = document.createElement("h3");
    cropHead.textContent = crop.crop;
    cropHead.style.color = colorFor(crop.crop);
    parent.appendChild(cropHead);
    const list = document.createElement("ul");
    for (const field of crop.fields || []) {
      const item = document.createElement("li");
      const button = document.createElement("button");
      button.type = "button";
      button.className = "field-row";
      button.dataset.id = field.id;
      if (field.id === selected) button.classList.add("is-selected");
      button.textContent = fieldLabel(field, showCounty);
      button.addEventListener("click", () => selectField(field.id));
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
