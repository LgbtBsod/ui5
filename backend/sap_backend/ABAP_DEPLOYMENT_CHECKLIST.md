# ABAP Deployment Checklist

## Build and packaging

- [ ] Run `npm install` on the release branch.
- [ ] Run `npm run build` or `npm run build:dist` and archive the generated `dist/` content.
- [ ] Verify `Component-preload.js` and related preload assets are present in `dist/`.
- [ ] Confirm the deployed artifact corresponds to the same commit/tag as the Gateway transport.

## ABAP repository deployment

- [ ] Confirm the deployment target (BSP / ABAP repository object) and owning transport request.
- [ ] Load the built UI5 app with `/UI5/UI5_REPOSITORY_LOAD_HTTPN` or the approved pipeline equivalent.
- [ ] Preserve the productive bootstrap source rules; do not change externally constrained test bootstrap URLs as part of repository import.
- [ ] Confirm MIME paths, cache-buster info, and BSP application name after import.

## App index and cache handling

- [ ] Run `/UI5/APP_INDEX_CALCULATE` or the pipeline equivalent for the deployed app/component.
- [ ] Refresh cache-buster metadata and invalidate stale app index entries if required by the landscape process.
- [ ] Clear front-end server / browser caches according to the landscape runbook before smoke testing.

## Productive smoke entry

- [ ] Launch the productive URL / FLP tile that points to the deployed BSP.
- [ ] Confirm the app starts without missing preload resources or library load errors.
- [ ] Confirm the productive service root still resolves to `/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`.

## Rollback readiness

- [ ] Record the previous BSP/UI5 artifact version before import.
- [ ] Confirm who can re-import the previous version and reverse the transport if smoke fails.
- [ ] Keep a rollback decision point before opening the deployment to business users.
