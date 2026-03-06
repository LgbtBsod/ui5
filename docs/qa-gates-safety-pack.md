# QA Gates — Safety Pack

## Proposed focused gates
- `scripts/gates/sessionid-writes-gate.js`
  - Reject writes to `/sessionId` outside `Component.js`.
- `scripts/gates/autosave-lock-guard-gate.js`
  - Heuristic check that autosave path includes EDIT + LOCKED + dirty guards.

These gates are lightweight, focused, and intended for partial QA during lock/autosave refactors.
