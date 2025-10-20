# Contributing

Thanks for your interest in improving the multi-drone coverage optimizer! Please follow these guidelines to keep the project healthy.

## Getting Started

1. Fork the repository and create a branch named after the feature or bugfix.
2. Run `make setup` to install dependencies for both Haskell and Python components.
3. Before submitting a pull request, ensure `make test` passes locally.

## Code Style

- Haskell: follow the defaults enforced by `ormolu` and keep functions pure when possible. Document all public functions with Haddock comments.
- Python: run `ruff` and `black` in check mode. Add type hints for public functions and keep docstrings concise.
- Write unit tests or property tests for new functionality.

## Commit Messages

Use meaningful commit messages written in the imperative mood, e.g., `Add STC coverage planner`.

## Pull Requests

- Describe the motivation, approach, and testing performed.
- Link related issues.
- Expect review within a few business days.

## Code of Conduct

Please be respectful. Harassment or discrimination will not be tolerated.
