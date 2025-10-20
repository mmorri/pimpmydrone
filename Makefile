.PHONY: setup build test run viz haskell-build python-build clean

STACK ?= stack
UV ?= uv
PYTHON ?= python3
SEED ?= 1337

setup:
	$(UV) --version >/dev/null
	cd python && $(UV) sync
	cd haskell && $(STACK) setup

build: haskell-build python-build

haskell-build:
	cd haskell && $(STACK) build --copy-bins --local-bin-path ../haskell/bin

python-build:
	cd python && $(UV) run $(PYTHON) -m compileall src

test:
	cd haskell && $(STACK) test
	cd python && $(UV) run pytest

run:
	cd python && $(UV) run $(PYTHON) -m fleet.cli run --seed $(SEED)

viz:
	cd python && $(UV) run $(PYTHON) -m fleet.cli visualize \
		--map ../outputs/$(SEED)/map.json \
		--tours ../outputs/$(SEED)/tours_optimized.json \
		--png ../outputs/$(SEED)/path.png

clean:
	rm -rf outputs
