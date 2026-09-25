.PHONY: setup build haskell-build python-build test lint format run viz loc clean

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

loc:
	python3 tools/loc_count.py

clean:
	rm -rf outputs haskell/bin
