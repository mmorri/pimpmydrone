# syntax=docker/dockerfile:1
# Base must match planner binary glibc (built on Ubuntu 26.04 / GLIBC 2.43).
FROM ubuntu:26.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        python3 python3-venv ca-certificates libgmp10 curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=ghcr.io/astral-sh/uv:latest /uv /usr/local/bin/uv

WORKDIR /app

COPY haskell/bin/coverage-partitioner haskell/bin/coverage-planner /app/haskell/bin/
COPY python/pyproject.toml python/README.md python/uv.lock /app/python/
COPY python/src /app/python/src
COPY website /app/website
COPY examples /app/examples
COPY LEGAL.md /app/LEGAL.md
COPY schema /app/schema

RUN chmod +x /app/haskell/bin/coverage-* \
    && cd /app/python \
    && uv python install 3.13 \
    && uv sync --frozen --no-dev --python 3.13 \
    && /app/haskell/bin/coverage-planner --help >/dev/null

ENV FLEET_ROOT=/app \
    FLEET_HASKELL_BIN=/app/haskell/bin \
    FLEET_WEBSITE=/app/website \
    FLEET_JOBS=/tmp/fleet-jobs \
    FLEET_MAX_CELLS=8000 \
    PATH="/app/python/.venv/bin:$PATH"

EXPOSE 8000
CMD ["uvicorn", "fleet.webapp:app", "--host", "0.0.0.0", "--port", "8000"]
