# Local Validation

This repo contains a real local validation path. Use the files in the repo; do not rely on deleted scripts or undocumented startup steps.

## Prerequisites

- Windows PowerShell
- Python 3
- For mock-backend startup: `fastapi` and `uvicorn` available in one of these interpreter sources, in this order:
  - `backend/mock_gateway/.venv`
  - `PYTHON_BIN`
  - `PYTHON`
  - `py -3`
  - `python`
- Node.js for repo gates and smoke runners

## Start local UI + mock Gateway

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1
```

Expected output:

- backend URL on port `8000`
- UI URL on port `8080`
- selected Python interpreter source
- log directory under `docs/runtime`

Stop the local environment:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/stop-local-env.ps1
```

## Start local UI against a real SAP Gateway

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1 -GatewayBaseUrl http://gateway-host:8000
```

This keeps the local UI server and points `/sap/*` traffic at the external Gateway.

## Automated smoke entrypoint

After startup succeeds:

```powershell
node scripts/gateway-live-smoke-runner.js
```

This runs the repo-native smoke pack:

- backend API smoke
- browser smoke for gateway-only flow
- browser smoke for detail attachment dirty invariant
- browser/domain facade smoke
- gateway smoke gates

If browser automation cannot run in your environment, use the manual smoke playbook below instead of claiming full automation.

## Manual smoke playbook

Open:

- UI: `http://127.0.0.1:8080/index.html`
- Gateway metadata: `http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/$metadata`

Validate these flows in order.

### 1. Startup

- App loads once and the shell leaves loading state.
- Search screen becomes usable without waiting for full analytics.
- No second global busy owner appears.

### 2. Search

- Search route opens immediately.
- Search table rebind works.
- Sticky/viewport behavior stays stable while scrolling.

### 3. Detail open and permission denial

- Open an existing checklist: detail starts in `READ`.
- Permission denial must show denied behavior without exposing cached business payload.

### 4. Edit and lock lifecycle

- Toggle edit: state becomes `EDIT` with lock state `EDIT_LOCKED`.
- If lock is lost, UI leaves edit-active behavior and shows lock-lost messaging.
- If idle timeout grace is triggered, UI shows grace messaging without pretending the lock is still healthy.
- Forced read-only path returns the detail flow to read-only semantics.

### 5. Save and autosave

- Save clears dirty state and leaves snapshot/selected aligned.
- Autosave only runs while `editMode=EDIT` and `lockState=EDIT_LOCKED`.
- After forced read-only or lock loss, autosave no longer continues.

### 6. Export

- Selected export only exports explicitly selected root IDs.
- All-found export follows the search contract, including `filterLocationKey`.
- Export is not limited to visible rows on the current table page.

### 7. Analytics

- Search page simple analytics load without blocking app readiness.
- Full analytics load only on the analytics route.

## Related validation assets

- `scripts/start-local-env.ps1`
- `scripts/stop-local-env.ps1`
- `scripts/gateway-live-smoke-runner.js`
- `scripts/gateway-only-smoke-pack.py`
- `scripts/browser-smoke-gateway-only-flow.py`
- `scripts/browser-smoke-detail-attachment-dirty-invariant.py`
- `backend/mock_gateway/README_ODATA.md`
