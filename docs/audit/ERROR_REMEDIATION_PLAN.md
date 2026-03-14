# Error Remediation Plan

Date: 2026-03-14

Purpose: convert the audit findings into a practical implementation sequence with clear priorities.

## P1

- SAP-system evidence gap:
  complete `EV-003`, `EV-004`, `EV-005`, `EV-006` using `TRACE_INVENTORY_EV003_EV006.md`.
- Productive runtime contour:
  switch UI5 bootstrap from temporary CDN back to `/resources/sap-ui-core.js` before Gateway/FLP deployment.
- Authorization and lock proofs:
  do not allow release sign-off until `OWNER_SIGNOFF_TRACKER.md` shows accepted ABAP, Basis/Gateway, Security, and architect sign-off.
- Concurrency and lock correctness:
  prove `If-Match`, stale lock cleanup, takeover, and heartbeat behavior on the real SAP landscape.

## P2

- Sticky UX hardening:
  execute `STICKY_KEYBOARD_MOBILE_CHECKLIST.md` and fix overlap/focus issues found on desktop and mobile widths.
- Smart control semantics:
  complete value help, labels, and field ordering validation against live Gateway metadata.
- Shell/settings communication:
  keep productive constraints explicit in UI and docs so test and product teams do not validate against the wrong contour.

## P3

- Theme system refinement:
  keep the current light theme, adopt the `oil-slick-bg-dev` dark background profile, and later decide whether dark mode is exposed in product UX or kept as controlled runtime capability.
- Qualification depth:
  expand QUnit/OPA from scaffold to scenario coverage for search/detail/sticky/a11y flows.
- Governance polish:
  keep evidence matrix, proof register, and owner sign-off tracker synchronized at each milestone.
