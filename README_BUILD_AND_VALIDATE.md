# Build and Validate

## Supported baseline
- Productive SAPUI5 baseline: `1.71.28`
- Local UI5 Tooling/framework package version: `1.71.70`

This repository intentionally keeps local tooling on `1.71.70` because `1.71.28` is not resolvable through the current UI5 framework package channel used by `ui5.yaml`. The productive deployment target remains `1.71.28`.

The test/bootstrap URLs are externally constrained and therefore may still reference `1.71.70`. Do not treat that as evidence that the productive rollout target changed.

## Local build
1. `npm install`
2. `npm run build`
3. `npm run build:dist`
4. `npm run validate`

## What validate does
- checks LF line endings
- runs CSS lint
- runs duplicate selector gate
- generates component preload output

## Dist / preload output
The standard UI5 build writes distributable output to `dist/`. The component preload bundle is generated during `npm run build:preload` / `npm run validate` and must be present in the build output used for ABAP deployment.

## Mandatory productive checks on real 1.71.28 landscape
Because local tooling uses `1.71.70`, the following checks must still be executed against the real productive UI5 `1.71.28` contour before release:
- startup and routing smoke
- search -> detail navigation
- analytics load and export
- save / autosave
- lock acquire / heartbeat / release
- attachment upload / download
- batch + CSRF refresh

## ABAP / Gateway rollout
Use the runbooks in `backend/sap_backend/` for:
- Gateway activation
- metadata parity
- ABAP deployment
- productive smoke testing
