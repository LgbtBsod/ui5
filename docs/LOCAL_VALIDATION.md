# Local Validation

## Bootstrap

- Start the local stack with `scripts/start-local-env.ps1`.
- Stop the local stack with `scripts/stop-local-env.ps1`.
- Run gateway smoke checks with `node scripts/gateway-live-smoke-runner.js`.

## Manual smoke playbook

- Verify edit flow acquires the lock and the state switches to `EDIT_LOCKED`.
- Verify search filters still propagate `filterLocationKey` into export and navigation flows.
- Verify create, save, autosave, heartbeat, and release all work against the local gateway.
- Verify attachment upload policy for allowed files and over-limit rejection.
