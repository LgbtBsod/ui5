# Local Validation

Architectural baseline for the current production-grade baseline.

## Start

Use `scripts/start-local-env.ps1` to launch the local UI and mock Gateway stack.
Use `scripts/stop-local-env.ps1` to stop the local environment cleanly.
Use `scripts/local-launch-smoke.ps1 -EnsureEnvironment` as the canonical deterministic local smoke entrypoint for automation and release checks.
Do not use `-BindToParentShell` for automated smoke runs. That mode is only for interactive shells and intentionally tears down the environment when the parent shell exits.
For live Gateway verification use `node scripts/gateway-live-smoke-runner.js`.
Run `npm run build` before smoke. In this local environment the build uses a repo-owned dist/preload fallback because public npm does not resolve SAPUI5 `1.71.70` framework metadata.

## Browser And Platform

Validate only in evergreen Microsoft Edge.
Internet Explorer is not supported.
Desktop is the only production target.
Tablet and phone rendering remain non-blocking best-effort and are not part of the formal release baseline.

## Manual smoke playbook

1. Open search and confirm initial load without console/runtime errors.
2. Filter by `filterLocationKey` and execute search.
3. Open detail, enter edit, and confirm lock state becomes `EDIT_LOCKED`.
4. Create a new checklist and complete the first save.
5. Add or remove an attachment, save again, and confirm the change survives reload without duplicate or ghost attachment state.
6. Reload search, reopen detail, and verify attachments, lock recovery, and analytics export.

## Deterministic local smoke

1. Run `cmd /c npm.cmd run validate:local`.
2. Run `cmd /c npm.cmd run validate:contracts`.
3. Run `powershell -ExecutionPolicy Bypass -File scripts/local-launch-smoke.ps1 -EnsureEnvironment`.
4. Run `py -3 scripts/browser-smoke-domain-facade-contract.py http://127.0.0.1:8080/index.html`.
5. Execute the manual smoke playbook in Microsoft Edge against the same local environment.

## Notes

Manual smoke playbook must be executed after lint, tests, and preload/build validation.
Phase 1 release readiness additionally requires `validate:contracts` to pass with the canonical attachment delta flow and reduced UI5 compatibility override surface.
