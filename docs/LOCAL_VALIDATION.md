# Local Validation Guide

## Architectural baseline

Use the local validation flow to prove the production-grade UI5 and mock Gateway contracts before release. The baseline covers the SAPUI5 runtime, OData mock Gateway, lock lifecycle, search/export contracts, and manual UX smoke checks in evergreen Microsoft Edge.

## Automated local environment

1. Start the local stack with `scripts/start-local-env.ps1`.
2. Execute the Gateway live smoke pack with `node scripts/gateway-live-smoke-runner.js`.
3. Stop the stack with `scripts/stop-local-env.ps1`.

## Contract checks

- Lock lifecycle must expose `EDIT_LOCKED` while an edit session owns the checklist.
- Search/export requests must preserve the canonical `filterLocationKey` predicate.
- The automated gates must run before manual acceptance.

## Manual smoke playbook

- Open the application in evergreen Microsoft Edge.
- Create a new checklist and complete the first save.
- Reopen the saved checklist, enter edit mode, verify the lock banner, save, and leave edit mode.
- Run a location-filtered search and export both selected rows and all found rows.
