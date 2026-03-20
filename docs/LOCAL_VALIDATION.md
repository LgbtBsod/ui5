# Local Validation

Use the local validation flow as the release gate for the full repo shape.

## Startup

- `scripts/start-local-env.ps1`
- `scripts/stop-local-env.ps1`

## Frontend and backend checks

- `cmd /c npm.cmd run validate`
- `python -m pytest tests -q`
- `node scripts/gateway-live-smoke-runner.js`

## Manual smoke playbook

Manual smoke playbook:

1. Open search and confirm filters still serialize `filterLocationKey`.
2. Open detail in read mode and then enter edit.
3. Confirm lock state becomes `EDIT_LOCKED`.
4. Save, close detail, reopen, and confirm no stale lock remains.
5. Copy an existing checklist and verify the new object opens with the expected state.
