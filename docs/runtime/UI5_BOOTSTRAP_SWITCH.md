# UI5 Bootstrap Switch

The repository supports two bootstrap modes for `app/ui5-bootstrap-runtime.js`.

## Local Development

Use the public UI5 CDN when you need the app to run outside an SAP FLP or Gateway runtime:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/set-ui5-bootstrap.ps1 -Mode cdn
```

This sets the bootstrap source to `https://ui5.sap.com/1.71.70/resources/sap-ui-core.js`.

## SAP Gateway Or FLP Deployment

Before packaging for the target SAP system, switch back to the system runtime:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/set-ui5-bootstrap.ps1 -Mode system
```

This sets the bootstrap source to `/resources/sap-ui-core.js`.

## Review Expectation

- `cdn` mode is a local-development concession only.
- `system` mode is the intended productive contour for SAP review, FLP launch, and Gateway deployment.
- Treat this switch as a release checklist item, not as an optional cleanup.
