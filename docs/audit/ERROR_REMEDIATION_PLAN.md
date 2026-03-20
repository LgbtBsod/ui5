# ERROR REMEDIATION PLAN

## P1

- Prove the real attachment UI flow end-to-end: `upload -> open/download -> delete -> save-reconcile` on existing and create-draft detail flows.
- Keep status buttons on one product path only: validate fields, patch entity status, send ordinary `SaveChanges`; no separate status command or use case may return.
- Continue cleaning route/root ownership so `selectedId` and `activeObjectId` are not treated as uncontrolled parallel truths outside the sanctioned root resolver and route sync path.

## P2

- Do not change lock transport from query to body until backend confirms support and the local Gateway contour proves `create -> edit -> heartbeat -> save/autosave -> release` on the new contract.
- Decide whether `GatewayBackendService` remains a documented backend facade or is removed in the next Gateway refactor wave; do not keep it as an undocumented proxy layer.
- Add OData annotation datasource only if the backend actually exposes annotations; otherwise keep this as an explicit Gateway dependency.
- Start targeted `$select` rollout on high-volume search/detail reads instead of broad speculative optimization.
- Decide preload strategy explicitly: real productive preload build path or documented dev-only placeholder contract for `Component-preload.js`.

## P3

- Refresh the product audit against the current repo state and keep `P1/P2` limited to live product issues, not already-closed DOM/style/sprawl findings.
- Continue reducing route/runtime indirection only where it improves supportability without reopening validated lifecycle flows.
