# Local Validation

This guide is the single local validation entry point for the current repo.

## Start The Local Environment

Default mock contour:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1
```

External Gateway contour:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-local-env.ps1 -GatewayBaseUrl "https://your-gateway-host"
```

Stop the local environment:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/stop-local-env.ps1
```

## Automated Checks

Architecture gate:

```powershell
node scripts/architecture-gate.js --json
```

Gateway-only readiness gate:

```powershell
node scripts/sap-gateway-only-gate.js --json
```

Enterprise readiness gate:

```powershell
node scripts/enterprise-readiness-gate.js scripts/enterprise-readiness-thresholds.json --json
```

Python backend tests:

```powershell
python -m pytest backend/mock_gateway/tests -q
```

Gateway live smoke runner:

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
4. Change a field and confirm save or autosave feedback is visible.
5. Exit detail and re-open to confirm state consistency.

### Attachment flow

1. Open a checklist with attachments enabled.
2. Verify upload policy is available before upload.
3. Upload a permitted file.
4. Verify attachment list refreshes without blocking the rest of the screen.

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

- Do not change the `sap-ui-core.js` source as part of this validation pass.
- The target productive service root remains `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`.
- Manual validation should prioritize `search`, `detail`, `EDIT_LOCKED`, export, analytics drilldown, and `filterLocationKey` continuity.
