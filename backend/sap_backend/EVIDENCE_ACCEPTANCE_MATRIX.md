# Evidence Acceptance Matrix

Date: 2026-03-27
Stack baseline: SAPUI5 1.71.70, SAP_UI 754, BASIS 750 SP15, HANA 2 SP6, SAP Gateway, evergreen Microsoft Edge
Release policy: release evidence must confirm the productive contract that already exists in code and gates.

| Evidence ID | Scope | Required proof | Current source of truth | Acceptance rule | Status |
| --- | --- | --- | --- | --- | --- |
| EV-003 | Lock lifecycle | Acquire, heartbeat, release over canonical `DB_KEY` contract | `scripts/gateway-only-smoke-pack.py`, `scripts/lock-contract-naming-gate.js`, productive Gateway trace | Same session owns lock, heartbeat refreshes it, release completes without alias drift | READY |
| EV-004 | Attachment architecture | Media upload to `AttachmentSet` and persisted binary access through `DownloadUrl` / `DocumentHandle` | `scripts/attachment-contract-gate.js`, `scripts/gateway-only-smoke-pack.py`, `backend/mock_gateway/README_ODATA.md` | No productive base64 save path, no binary payload field in canonical metadata, media upload remains the only binary ingress | READY |
| EV-005 | Search/detail runtime | Search open, detail navigation, save/edit-readonly transitions, canonical `DB_KEY` semantics | `scripts/ui-runtime-audit.py`, `scripts/key-model-gate.js`, `scripts/final-residual-cleanup-gate.js` | Search/detail flows keep canonical internal key semantics without legacy alias leakage | READY |
| EV-006 | Productive runtime baseline | Confirmed UI5 runtime source and release remediation state | `docs/audit/PRODUCTIVE_UI5_RUNTIME.md`, `docs/audit/ERROR_REMEDIATION_PLAN.md`, `scripts/release-readiness-gate.js` | Runtime baseline is confirmed, remediation sections exist, release docs are present and internally consistent | READY |

## Notes

- The evidence matrix is a release artifact, not a substitute for the smoke checklist.
- Productive sign-off still requires live Gateway / ABAP evidence from the target landscape.
- Any release candidate that reintroduces legacy key aliases, base64 attachment persistence, or stale UI5 runtime assumptions invalidates this matrix.
