# Error Remediation Plan

Date: 2026-03-27
Status: active release remediation baseline

## P1

- Keep release blockers at zero for canonical key model, lock naming, attachment media architecture, raw UI text ownership, wrapper sprawl, and SAP internal CSS gate.
- Reject any regression that reintroduces `RootKey`, `RootId`, `ObjectUuid`, `Attachment.Value`, or productive base64 attachment persistence.
- Require release evidence docs and owner tracker to stay present and aligned with current code and gates.

## P2

- Continue shrinking SAP internal CSS/DOM debt in `detail`, `search`, `dialogs`, and shared control bundles.
- Reduce DOM-hack allowlist entries only when UI5 1.71 public APIs fully cover focus, geometry, and selection behavior.
- Keep artifact docs synchronized with the real productive model instead of roadmap-era transitional semantics.

## P3

- Continue post-release simplification of quarantined UI5 selectors and DOM runtimes.
- Keep mock Gateway parity checks aligned with productive ABAP behavior.
- Treat additional bootstrap/runtime simplification as maintenance work unless it becomes a release blocker.
