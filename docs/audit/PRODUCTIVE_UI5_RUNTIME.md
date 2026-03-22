# Productive UI5 Runtime Baseline

Status: CONFIRMATION_REQUIRED

This project intentionally keeps `ui5.yaml` and `manifest.json` aligned to the local build baseline until the productive SAP Gateway runtime is confirmed.

Required productive evidence before release:

- Endpoint: `/sap/public/bc/ui5_ui5/1/resources/sap-ui-version.json`
- Environment: productive SAP Gateway / target landscape
- Confirmed SAPUI5 version: `TBD`
- Confirmed on: `TBD`
- Confirmed by: `TBD`

Release rule:

- `npm run validate:local` is developer-grade validation and may skip the real SAPUI5 preload build.
- `npm run validate:release` is release-grade validation and must only pass after this document is updated with the confirmed productive UI5 runtime version.
