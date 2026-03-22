# Local Validation

Architectural baseline for the current production-grade baseline.

## Start

Use `scripts/start-local-env.ps1` to launch the local UI and mock Gateway stack.
Use `scripts/stop-local-env.ps1` to stop the local environment cleanly.
For live Gateway verification use `node scripts/gateway-live-smoke-runner.js`.
Run `npm run build` before smoke. In this local environment the build uses a repo-owned dist/preload fallback because public npm does not resolve SAPUI5 `1.71.70` framework metadata.

## Browser And Platform

Validate only in evergreen Microsoft Edge.
Internet Explorer is not supported.

## Manual smoke playbook

1. Open search and confirm initial load without console/runtime errors.
2. Filter by `filterLocationKey` and execute search.
3. Open detail, enter edit, and confirm lock state becomes `EDIT_LOCKED`.
4. Create a new checklist and complete the first save.
5. Reload search, reopen detail, and verify attachments, lock recovery, and analytics export.

## Notes

Manual smoke playbook must be executed after lint, tests, and preload/build validation.
