# Contributing

Thanks for your interest in improving the multi-drone coverage optimizer! Please follow these guidelines to keep the project healthy.

## Getting Started

1. Fork the repository and create a branch named after the feature or bugfix.
2. Run `make setup` to install dependencies for both Haskell and Python components, then `make build`.
3. Before submitting a pull request, ensure `make test` and `make lint` pass locally. The end-to-end
   Python tests exercise the Haskell binaries, so build them first.

## Code Style

- Haskell: the library and executables build with `-Wall`; keep them warning free. Prefer pure functions and document public functions with Haddock comments.
- Python: `make format` runs `black` and `ruff --fix`. Add type hints for public functions and keep docstrings concise.
- Write unit tests or property tests for new functionality. Changes to the JSON contracts must update `schema/` as well.

## Commit Messages

Use meaningful commit messages written in the imperative mood, e.g., `Add STC coverage planner`.

## Pull Requests

- Describe the motivation, approach, and testing performed.
- Link related issues.
- Expect review within a few business days.

## Code of Conduct

Please be respectful. Harassment or discrimination will not be tolerated.
