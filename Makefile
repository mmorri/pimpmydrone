.PHONY: setup build haskell-build python-build test lint format run viz bench loc clean serve site

STACK ?= stack
UV ?= uv
SEED ?= 1337
ARGS ?=

setup:
	$(UV) --version >/dev/null
	cd python && $(UV) sync
	cd haskell && $(STACK) setup

build: haskell-build python-build

haskell-build:
	cd haskell && $(STACK) build --copy-bins --local-bin-path bin

python-build:
	cd python && $(UV) run python -m compileall -q src

test:
	cd haskell && $(STACK) test
	cd python && $(UV) run pytest

lint:
	cd python && $(UV) run ruff check src tests && $(UV) run black --check src tests

format:
	cd python && $(UV) run black src tests && $(UV) run ruff check --fix src tests

# Full pipeline: generate -> partition -> plan -> optimize -> simulate -> visualize.
# Extra CLI flags go through ARGS, e.g. make run SEED=7 ARGS="--battery 400 --depots 2".
run:
	cd python && $(UV) run python -m fleet run --seed $(SEED) $(ARGS)

viz:
	cd python && $(UV) run python -m fleet visualize \
		--map ../outputs/$(SEED)/map.json \
		--tours ../outputs/$(SEED)/tours_optimized.json \
		--png ../outputs/$(SEED)/path.png

# Mission matrix reporting makespan, flight length and commute share.
# ARGS is passed through, e.g. make bench ARGS="--compare benchmarks/baseline.json"
bench:
	cd python && $(UV) run python ../tools/bench.py $(ARGS)

loc:
	python3 tools/loc_count.py

# Local product UI: draw/upload GeoJSON → plan → download QGC missions.
serve:
	cd python && $(UV) run python -m fleet serve --host 127.0.0.1 --port 8000

# Example vertical site (solar by default). Override with e.g.
#   make site GEO=examples/sites/security_perimeter.geojson VERTICAL=security
GEO ?= examples/sites/solar_farm.geojson
VERTICAL ?= solar
SITE_OUT ?= outputs/site-$(VERTICAL)
site:
	cd python && $(UV) run python -m fleet from-geo \
		--geojson ../$(GEO) \
		--output-dir ../$(SITE_OUT) \
		--vertical $(VERTICAL)

clean:
	rm -rf outputs haskell/bin
