# Final production baseline

This document captures the production-grade baseline for the UI5 application.

- Architectural constants in `app/constants/*` are authoritative.
- `ModelPathContracts` and `StatePaths` are the canonical model-path contract surfaces.
- Use cases are plain modules/factories exposing `{ execute }`.
- Supported browser baseline: evergreen Microsoft Edge.
