# UI Resurrection Notes

## Phase 0 triage findings
1. Lock-loss downgrade was executed directly in `Component.js` with mixed state mutations/timer stops, bypassing domain-level workflow.
2. Enter-edit lock acquisition did not enforce `sessionGuid` from state, so lock/session safety contract could be violated.
3. Autosave preconditions relied on partial checks and did not explicitly bind to lock state constant, making UI state drift possible.
4. Confirm actions for takeover could degrade UX when warning+confirm were emitted together.

## Fix summary
- Introduced session-id startup guard and constant-based access.
- Added `LockLostUseCase` + facade orchestration for lock-lost downgrade effects.
- Added takeover confirm flow via `Effects.confirm` + `ActionDispatcher`.
- Enforced autosave guardrail in coordinator (`guardFn`) and component predicate.
- Simplified locked-own-session path to emit confirm-only takeover UX without duplicate warning modal.

- Follow-up: removed one remaining legacy startup write to `/sessionId` and switched to `fnEnsureSessionId()` for strict idempotence.

- Follow-up: lock status/heartbeat now treats `ok=false` (without `is_killed`) as lock-lost and routes downgrade via `DetailFacade.onLockLost` to stop autosave/read-only consistently.
