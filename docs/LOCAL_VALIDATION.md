# Local Validation

Use the local validation flow as the release gate for the full repo shape.

Current browser baseline: evergreen Microsoft Edge. Internet Explorer is out of scope.

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
2. Open detail in read mode, enter edit, and confirm lock state becomes `EDIT_LOCKED`.
3. Save, close detail, reopen, and confirm no stale lock remains.
4. Copy an existing checklist, confirm the new object opens correctly, and complete the first save.
5. Create a new checklist and complete the first save.
6. Reopen the copied or created checklist and confirm the state remains consistent.
7. Sanity-check shell/search sticky and layout behavior on the current Edge baseline.

## Architectural baseline

Release-ready baseline flow:

1. `search -> detail -> edit -> save`
2. `close -> reopen` without stale lock
3. `copy/create -> first save -> reopen`
4. shell/search sticky and layout runtime remain stable on evergreen Edge
