# Local Validation

This guide is the single local validation entry point for the current repo.

## Start The Local Environment

Default mock contour:

```bat
start.bat
```

This launcher starts the mock backend in `PCCT_PROFILE=local`, which keeps mock identity and startup seeding enabled only for local validation.

External Gateway contour:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1 -GatewayBaseUrl "https://your-gateway-host"
```

For a hardened mock contour without startup mutation or mock-header identity, start the backend manually with:

```powershell
$env:PCCT_PROFILE="preprod"
$env:PCCT_ALLOW_MOCK_USER_HEADER="0"
$env:PCCT_AUTO_MUTATE_SCHEMA="0"
$env:PCCT_AUTO_SEED_STARTUP_DATA="0"
python -m uvicorn main:app --host 127.0.0.1 --port 8000
```

Stop the local environment:

```bat
start.bat stop
```

Check status or clean derived local artifacts:

```bat
start.bat status
start.bat clean
```

## Automated Checks

Install the frontend toolchain:

```powershell
npm install
```

Set the bootstrap source for local development:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/set-ui5-bootstrap.ps1 -Mode cdn
```

Build the UI5 artifact:

```powershell
npm run build
```

Run the unit-test entry point:

```powershell
npm run test:unit
```

Run the OPA smoke entry point:

```powershell
npm run test:opa
```

Run the repo-side release readiness gate:

```powershell
npm run gate:release
```

Run the local launch smoke once the environment is up:

```powershell
npm run smoke:local
```

Run Python backend tests:

```powershell
python -m pytest backend/mock_gateway/tests -q
```

Run the live Gateway smoke helper when validating against a real SAP system:

```powershell
node scripts/gateway-live-smoke-runner.js
```

## Manual smoke playbook

### Search flow

1. Open the app.
2. Confirm search renders without a global hard block.
3. Execute a search with a known result set.
4. Confirm filtering and result selection work.
5. Confirm `filterLocationKey` survives search and export flow.

### Detail flow

1. Open a checklist from search results.
2. Confirm the detail pane loads independently.
3. Enter edit mode and verify lock state becomes `EDIT_LOCKED`.
4. Change a root field and confirm save or autosave feedback is visible.
5. Verify save/autosave payload contains explicit `edit_mode` markers for changed rows.
6. Exit detail and re-open to confirm state consistency.

### Attachment flow

1. Open a checklist with attachments enabled.
2. Verify upload policy is available before upload.
3. Upload a permitted file.
4. Verify attachment create is represented in the delta contract under `attachments[]` with `edit_mode = C`.
5. Delete an attachment and verify delete is represented in the delta contract under `attachments[]` with `edit_mode = D`.
6. Verify attachment list refreshes without blocking the rest of the screen.

### Analytics flow

1. Open analytics after the main search/detail flow is usable.
2. Change year and compare year.
3. Trigger a drilldown and confirm the search context receives it correctly.
4. Confirm analytics does not block primary checklist work.

### External Gateway mode

1. Start the app with `-GatewayBaseUrl`.
2. Confirm metadata loads from the Gateway service root.
3. Confirm CSRF-protected write flows behave correctly.
4. Confirm search, detail, lock, save, and export flows still work.

## Notes

- The current local contour intentionally uses `https://ui5.sap.com/1.71.70/resources/sap-ui-core.js` for the app and QUnit/OPA pages.
- The default local launch path is now:
  - `start.bat`
  - `npm run smoke:local`
- Before deployment to the real SAP Gateway or FLP runtime, switch bootstrap back to the system runtime and remove the public CDN dependency.
- The supported switch procedure is documented in [UI5_BOOTSTRAP_SWITCH.md](/C:/Users/lgbtb/Desktop/ui5/docs/runtime/UI5_BOOTSTRAP_SWITCH.md).
- The controlled QA/dev dark-mode override is documented in [DARK_THEME_OVERRIDE.md](/C:/Users/lgbtb/Desktop/ui5/docs/runtime/DARK_THEME_OVERRIDE.md).
- The target productive service root remains `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`.
- Manual validation should prioritize `search`, `detail`, `EDIT_LOCKED`, export, analytics drilldown, and `filterLocationKey` continuity.
- Readiness telemetry is written under `state>/readiness/metrics/stages/*` for `shellReady`, `searchRouteReady`, `searchInteractionReady`, `detailReady`, `analyticsReady`, and `deferredDialogReady`.
- The sanctioned mutable payload contract is now unified and delta-first:
  - `root/checks/barriers/participants/attachments/client_version`
  - mutable rows must carry `edit_mode = C|U|D`
- Legacy architecture and QA gates are retained as supplementary diagnostics only. They are not the release source of truth for SAP sale-readiness.
