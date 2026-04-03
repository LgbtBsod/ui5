# Productive UI5 Runtime

Status: CONFIRMED
Confirmed SAPUI5 version: `1.71.70`
Confirmed productive source: `"/resources/sap-ui-core.js"`
Fallback local development source: `window.__ui5BootstrapSrc`
Browser support policy: evergreen Microsoft Edge only, no Internet Explorer path

## Baseline

- Productive runtime stays on the SAP-hosted UI5 runtime exposed through `/resources/sap-ui-core.js`.
- Local development may override the bootstrap source temporarily, but productive validation assumes the SAP runtime path.
- Compatibility mode stays `edge`.
- Theme baseline stays `sap_fiori_3`.

## Release expectations

- Any productive deployment must prove that `/resources/sap-ui-core.js` resolves correctly in the target landscape.
- No release candidate may depend on public CDN runtime as a productive default.
- Browser behavior must be validated in current Microsoft Edge, not IE fallback branches.
