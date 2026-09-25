# fleet

Python side of the multi-drone coverage optimizer: map generator, CP-SAT tour scheduler, simulator, visualizer and the `fleet` command line.

```bash
uv sync
uv run python -m fleet --help
uv run pytest            # end-to-end tests run when ../haskell/bin is built
uv run ruff check src tests && uv run black --check src tests
```

See the root `README.md` for the full pipeline.
