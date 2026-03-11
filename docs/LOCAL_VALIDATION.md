# Local Validation

This repository supports local validation against either the bundled mock Gateway or an external SAP Gateway.

## Prerequisites

- PowerShell on Windows.
- Python 3 with `fastapi`, `uvicorn`, and `playwright` available in one of the supported interpreter locations when running the mock backend.
- Node.js for JS gates and the smoke runner.
- Playwright browser binaries installed for browser smoke.

## Python interpreter resolution

`scripts/start-local-env.ps1` resolves Python in this order:

1. `backend/mock_gateway/.venv`
2. `PYTHON_BIN`
3. `PYTHON`
4. `py -3`
5. `python`

If mock-backend startup is requested, the selected interpreter must have `fastapi` and `uvicorn`.

## Startup sequence

Mock Gateway mode:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1
```

External Gateway mode:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1 -GatewayBaseUrl "https://<gateway-host>"
```

Stop the local environment:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/stop-local-env.ps1
```

## Backend-only smoke

Run the API smoke plus browser-smoke pack wrapper:

```powershell
node scripts/gateway-live-smoke-runner.js
```

This executes:

- `scripts/gateway-only-smoke-pack.py`
- `scripts/gateway-only-smoke-gate.js`
- `scripts/gateway-smoke-invariant-gate.js`

## Browser smoke entrypoints

Facade contract smoke:

```powershell
python scripts/browser-smoke-domain-facade-contract.py http://127.0.0.1:8080/index.html
```

Gateway-only browser flow:

```powershell
python scripts/browser-smoke-gateway-only-flow.py http://127.0.0.1:8080/index.html <ROOT_ID>
```

Attachment dirty invariant:

```powershell
python scripts/browser-smoke-detail-attachment-dirty-invariant.py http://127.0.0.1:8080/index.html <ROOT_ID>
```

## Contract and gate checks

```powershell
node scripts/gates/lock-state-enum-gate.js
node scripts/gates/autosave-lock-guard-gate.js
python -m pytest backend/mock_gateway/tests/test_gateway_contract_frontend_aliases.py backend/mock_gateway/tests/test_closeout_invariants.py -q
```

## Fully passing scenarios

Current fully passing validation from repo-native tooling:

- startup through `scripts/start-local-env.ps1`
- backend API smoke in `scripts/gateway-only-smoke-pack.py`
- facade contract smoke in `scripts/browser-smoke-domain-facade-contract.py`
- JS gates:
  - `scripts/gates/lock-state-enum-gate.js`
  - `scripts/gates/autosave-lock-guard-gate.js`
- backend contract/invariant pytest suite

## Currently unstable scenarios

- `scripts/browser-smoke-gateway-only-flow.py`
  - current issue class: browser page/context lifecycle instability during later post-save flow
- `scripts/browser-smoke-detail-attachment-dirty-invariant.py`
  - current issue class: attachment section/browser readiness instability after successful `EDIT_LOCKED` acquisition

These scenarios are not green unless their report explicitly finishes with `"ok": true`.

## Manual smoke playbook

Use this sequence when browser automation is partially failing:

1. Start the environment with `scripts/start-local-env.ps1`.
2. Open `http://127.0.0.1:8080/index.html`.
3. Verify startup:
   - shell renders
   - search route opens
   - app is usable without waiting for analytics
4. Open a checklist:
   - detail opens read-only first
   - no business payload is exposed on denied permission
5. Enter edit:
   - switch changes to edit
   - lock state becomes `EDIT_LOCKED`
6. Save and autosave:
   - save completes
   - autosave reaches `SAVED`
   - leaving edit or losing lock stops active edit-only behavior
7. Export:
   - selected export uses selected ids only
   - all-found export uses `SearchContract`, including `filterLocationKey`
8. Analytics:
   - analytics screen loads only after navigating to the analytics route

## Runtime logs

Local runtime logs are written under `docs/runtime/`:

- `mock_backend.out.log`
- `mock_backend.err.log`
- `ui_server.out.log`
- `ui_server.err.log`

These files are generated locally and must not be committed.

## Generated artifacts that must not be committed

- `backend/mock_gateway/gateway.db`
- `backend/mock_gateway/uploads/*`
- `__pycache__/`
- `*.pyc`
- `.pytest_cache/`
- `docs/runtime/*.log`
- `docs/runtime/*.pid`
- `docs/runtime/*.txt`
- `docs/artifacts/*`
- Playwright/browser output folders
