# Local Validation

This repository supports local validation against either the bundled mock Gateway or an external SAP Gateway.

## Prerequisites

- Windows PowerShell.
- Python 3.
- `fastapi` and `uvicorn` in the selected Python interpreter when using the bundled mock backend.
- `playwright` in the selected Python interpreter for browser smoke.
- Node.js for JS gates and smoke orchestration.
- Installed Playwright browser binaries for browser smoke.

## Python fallback order

`scripts/start-local-env.ps1` resolves Python in this order:

1. `backend/mock_gateway/.venv`
2. `PYTHON_BIN`
3. `PYTHON`
4. `py -3`
5. `python`

If the bundled mock backend is started, the selected interpreter must provide `fastapi` and `uvicorn`.

## Startup commands

Bundled mock Gateway:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1
```

External SAP Gateway:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1 -GatewayBaseUrl "https://<gateway-host>"
```

Stop the local environment:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/stop-local-env.ps1
```

## Backend-only smoke commands

```powershell
python scripts/gateway-only-smoke-pack.py http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV
```

Aggregate smoke runner:

```powershell
node scripts/gateway-live-smoke-runner.js
```

## Browser smoke commands

Facade smoke:

```powershell
python scripts/browser-smoke-domain-facade-contract.py http://127.0.0.1:8080/index.html
```

Gateway-only flow:

```powershell
python scripts/browser-smoke-gateway-only-flow.py http://127.0.0.1:8080/index.html <ROOT_ID>
```

Attachment dirty invariant:

```powershell
python scripts/browser-smoke-detail-attachment-dirty-invariant.py http://127.0.0.1:8080/index.html <ROOT_ID>
```

## Gate commands

```powershell
node scripts/gates/lock-state-enum-gate.js
node scripts/gates/autosave-lock-guard-gate.js
python -m pytest backend/mock_gateway/tests/test_gateway_contract_frontend_aliases.py backend/mock_gateway/tests/test_closeout_invariants.py -q
```

## Validation status categories

### Fully passing

- `scripts/start-local-env.ps1`
- backend API smoke in `scripts/gateway-only-smoke-pack.py`
- facade contract smoke in `scripts/browser-smoke-domain-facade-contract.py`
- `scripts/gates/lock-state-enum-gate.js`
- `scripts/gates/autosave-lock-guard-gate.js`
- contract/invariant pytest tests
- `scripts/sap-gateway-only-gate.js`
- `scripts/runtime-settings-gate.js`

### Partially failing

- `scripts/browser-smoke-gateway-only-flow.py`
  - current failing step: `analytics.close`
  - current reason: detail route does not return to a stable `EDIT` + `EDIT_LOCKED` state within the current smoke contract after closing analytics
- `scripts/browser-smoke-detail-attachment-dirty-invariant.py`
  - current failing step: `attachments.upload`
  - current reason: staged attachment does not satisfy the local projection or dirty-state contract expected by the smoke script before explicit save

### Not executed

- Manual browser walk-through from this guide when local UI validation is delegated to a human run instead of current-session automation.

## Manual smoke playbook

Use the startup command, then walk these checkpoints in the running UI.

## Runtime logs

Generated local logs are written under `docs/runtime/`:

- `mock_backend.out.log`
- `mock_backend.err.log`
- `ui_server.out.log`
- `ui_server.err.log`

These files are generated locally and must not be committed.

## Generated artifacts that must not be committed

- `backend/mock_gateway/gateway.db`
- `backend/mock_gateway/gateway.db-shm`
- `backend/mock_gateway/gateway.db-wal`
- `backend/mock_gateway/uploads/*`
- `__pycache__/`
- `*.pyc`
- `.pytest_cache/`
- `docs/runtime/*.log`
- `docs/runtime/*.pid`
- `docs/runtime/*.txt`
- `docs/artifacts/*`
- Playwright/browser output folders

## Expected failure modes when dependencies are missing

- missing Python interpreter:
  - startup fails before backend/UI launch
- missing `fastapi` or `uvicorn`:
  - bundled mock-backend startup fails with dependency error
- missing `playwright`:
  - browser smoke scripts fail before scenario execution
- missing browser binaries:
  - browser smoke fails during Playwright launch
- wrong external Gateway URL:
  - metadata/startup probe fails during `start-local-env.ps1`

1. Startup:
   - app shell renders
   - search route opens
2. Detail:
   - detail opens read-only first
   - permission denial does not reveal business payload
3. Edit and lock:
   - edit enters `EDIT_LOCKED`
   - read-only downgrade is visible after lock loss or forced read-only
4. Save and autosave:
   - save completes
   - autosave reaches `SAVED`
5. Export:
   - selected export uses selected ids only
   - all-found export uses `SearchContract`, including `filterLocationKey`
6. Analytics:
   - analytics loads only on the analytics route

## Known unstable step handling

- If `scripts/browser-smoke-detail-attachment-dirty-invariant.py` fails at `attachments.upload`:
  - inspect `docs/artifacts/gateway-browser-attachment-dirty-report.json`
  - confirm whether the staged row appeared in `selected>/attachments` or `view>/sessionAttachments`
  - confirm there was no immediate `SaveChanges`, `CreateChecklist`, `AutoSave`, or `AttachmentSet` transport during staging
- If `scripts/browser-smoke-gateway-only-flow.py` fails at `analytics.close`:
  - inspect `docs/artifacts/gateway-browser-smoke-report.json`
  - verify whether the app returned to app route `detail` and restored detail state `EDIT` + `EDIT_LOCKED`
  - treat route-return instability separately from save/autosave/attachment-save transport, which should already be green before this step
