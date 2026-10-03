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
  };

  const map = L.map(mapEl).setView([36.5, -119.6], 8);
  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    attribution: "&copy; OpenStreetMap",
  }).addTo(map);

  const layers = new Map();
  let selected = "";
  let countyFilter = "All";

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

  function renderList(counties) {
    catalogEl.replaceChildren();
    for (const county of counties) {
      if (countyFilter !== "All" && county.county !== countyFilter) continue;
      const section = document.createElement("section");
      section.className = "field-county";
      const heading = document.createElement("h2");
      heading.textContent = county.county;
      section.appendChild(heading);
      for (const crop of county.crops || []) {
        const cropHead = document.createElement("h3");
        cropHead.textContent = crop.crop;
        cropHead.style.color = colorFor(crop.crop);
        section.appendChild(cropHead);
        const list = document.createElement("ul");
        for (const field of crop.fields || []) {
          const item = document.createElement("li");
          const button = document.createElement("button");
          button.type = "button";
          button.className = "field-row";
          button.dataset.id = field.id;
          if (field.id === selected) button.classList.add("is-selected");
          const acres = Number(field.acres);
          button.textContent = Number.isFinite(acres)
            ? `${acres.toFixed(1)} ac`
            : field.id;
          button.addEventListener("click", () => selectField(field.id));
          item.appendChild(button);
          list.appendChild(item);
        }
        section.appendChild(list);
      }
      catalogEl.appendChild(section);
    }
  }

  function renderFilters(counties) {
    if (!filtersEl) return;
    const names = ["All", ...counties.map((row) => row.county)];
    filtersEl.replaceChildren();
    for (const name of names) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "field-filter";
      button.textContent = name;
      button.setAttribute("aria-pressed", name === countyFilter ? "true" : "false");
      button.addEventListener("click", () => {
        countyFilter = name;
        filtersEl.querySelectorAll(".field-filter").forEach((el) => {
          el.setAttribute("aria-pressed", el.textContent === name ? "true" : "false");
        });
        renderList(counties);
        const visible = [...layers.values()].filter(
          (entry) => countyFilter === "All" || entry.county === countyFilter
        );
        layers.forEach((entry) => {
          const show = countyFilter === "All" || entry.county === countyFilter;
          if (show) entry.layer.addTo(map);
          else map.removeLayer(entry.layer);
        });
        if (visible.length) {
          const group = L.featureGroup(visible.map((entry) => entry.layer));
          map.fitBounds(group.getBounds(), { padding: [24, 24] });
        }
      });
      filtersEl.appendChild(button);
    }
  }

  async function load() {
    const res = await fetch("/api/fields");
    const catalog = await res.json();
    if (!res.ok) throw new Error(catalog.detail || "Could not load fields");
    if (sourceEl) {
      sourceEl.textContent = catalog.attribution || catalog.source || "";
    }
    const counties = catalog.counties || [];
    renderFilters(counties);
    renderList(counties);

    const jobs = [];
    for (const county of counties) {
      for (const crop of county.crops || []) {
        for (const field of crop.fields || []) {
          jobs.push({ county: county.county, crop: crop.crop, field });
        }
      }
    }
    const geos = await Promise.all(
      jobs.map(async (job) => {
        const geoRes = await fetch(`/api/fields/${encodeURIComponent(job.field.id)}`);
        const geo = await geoRes.json();
        if (!geoRes.ok) return null;
        return { ...job, geo };
      })
    );
    for (const row of geos) {
      if (!row) continue;
      const acres = Number(row.field.acres);
      const label = `${row.county} ${row.crop.toLowerCase()} · ${
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
      layers.set(row.field.id, { layer, crop: row.crop, county: row.county, label });
    }
    if (layers.size) {
      const group = L.featureGroup([...layers.values()].map((entry) => entry.layer));
      map.fitBounds(group.getBounds(), { padding: [24, 24] });
    }
  }

  load().catch((err) => {
    catalogEl.textContent = String(err.message || err);
  });
})();
