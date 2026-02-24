# Frontend ↔ backend integration (UI5 + mock_gate_way)

## Current status
- Frontend now supports **two backend modes** via `BackendAdapter`:
  - `fake` (default): in-memory mock for stable local development.
  - `real`: HTTP calls to `mock_gate_way` API.
- Mode and backend URL are configured through `manifest.json`.

## Configuration in `manifest.json`
- `sap.ui5.config.backendMode`
  - `"fake"` (default) or `"real"`
- `sap.app.dataSources.mainService.uri`
  - backend base URL (default: `http://localhost:5000`)

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
- exact status / KPI mapping parity between UI fake data and backend entities
- object-lock UX is still simplified on frontend side (auto acquire in backend adapter)

## Recommendation
For production-like end-to-end testing:
1. Run backend `mock_gate_way` on `http://localhost:5000`.
2. Switch `backendMode` to `real` in manifest (or use `?backend=real`).
3. Keep `fake` mode available for deterministic UI regression checks.


## Lock behavior (web-oriented, SAP-portable)
- Lock TTL is 5 minutes; heartbeat target interval is 4 minutes; cleanup interval is 5 minutes.
- `lock/acquire` supports same-session refresh and own-session steal (`iv_steal_from`).
- Heartbeat returns `is_killed`, `killed_by`, `server_changed_on`, and `version_number`.
- Beacon unload uses best-effort release (`sendBeacon`, fallback sync XHR).
