# Local Validation

## Architectural baseline
- Target browser baseline is evergreen Microsoft Edge.
- Productive lock smoke must verify `EDIT_LOCKED`.
- Search/export smoke must cover `filterLocationKey`.

## Scripts
- `scripts/start-local-env.ps1`
- `scripts/stop-local-env.ps1`
- `node scripts/gateway-live-smoke-runner.js`

## Manual smoke playbook
1. Start local services with `scripts/start-local-env.ps1`.
2. Open the app in evergreen Microsoft Edge.
3. Create a new checklist and complete the first save.
4. Run Search, Detail, Analytics, lock, and autosave smoke.
5. Stop local services with `scripts/stop-local-env.ps1`.
