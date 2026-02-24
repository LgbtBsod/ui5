# Frontend ↔ backend integration (UI5 + mock_gate_way)

## Current status
- Frontend now supports **two backend modes** via `BackendAdapter`:
  - `fake` (default): in-memory mock for stable local development.
  - `real`: HTTP calls to `mock_gate_way` API.
- Mode and backend URL are configured through `manifest.json`.

## Configuration in `manifest.json`
- `sap.ui5.config.backendMode`
  - `"fake"` (default) or `"real"`
- `sap.app.dataSources.mockGateway.uri`
  - backend base URL (default: `http://localhost:8000`)

You can also force runtime mode with URL query parameter:
- `?backend=real`
- `?backend=fake`

## Practical compatibility
### Fully aligned
- checklist list loading (`/checklist` + per-item details)
- persons dictionary loading (`/persons/suggest`)
- dictionaries (`/dict?domain=LPC|PROFESSION`)
- locations (`/location?date=YYYY-MM-DD`)
- lock heartbeat/release (`/lock/heartbeat`, `/lock/release`)

### Partial / needs backend expansion for full parity
- checklist delete flow (frontend currently expects hard delete endpoint)
- full checks/barriers row upsert semantics from object editor
- exact status / KPI mapping parity between UI fake data and backend entities

## Recommendation
For production-like end-to-end testing:
1. Run backend `mock_gate_way` on `http://localhost:8000`.
2. Switch `backendMode` to `real` in manifest (or use `?backend=real`).
3. Keep `fake` mode available for deterministic UI regression checks.
