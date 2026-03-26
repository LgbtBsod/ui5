# CSS DOM Violations

## Closed In This Pass
- No direct CSS/DOM rewrite was completed in the final production-readiness pass.
- Governance was still strengthened indirectly by keeping CSS/DOM debt explicit while closing attachment/lock/bootstrap blockers first.
- Removed stale SAP-selector allowlist entries for:
  - `app/styles/modules/controls/24_switches_and_toggles.css`
  - `app/styles/modules/controls/33_overflow_and_badges.css`
  - `app/styles/modules/controls/31_feedback_runtime.css`
- Tightened `scripts/sap-internal-css-gate.js` so unused allowlist entries fail instead of silently persisting.

## Remaining Active Private Selector Debt
- `app/styles/modules/controls/26_controls.css`

## Policy
- Prefer app-owned classes.
- Prefer public UI5 APIs and theme-safe wrappers.
- Keep allowlist entries only while a file still contains justified private selectors.
- New hard gates:
  - `scripts/sap-internal-css-gate.js` now scans the full `app/styles` tree
  - `scripts/dom-hack-gate.js` now flags DOM-dependent runtime outside the explicit allowlist
- Current blockers still concentrated in:
  - `app/styles/modules/23_dialogs.css`
  - `app/styles/modules/40_page_search.css`
  - `app/styles/modules/controls/34_action_priority_and_a11y.css`
  - `app/service/features/search/runtime/SearchViewportRuntime.js`
  - `app/controller/App.controller.js`
# CSS / DOM Status Delta

- Search and dialog modules still contain internal UI5 selector debt and remain on the remediation backlog.
- No new `.sap*` selector allowances were introduced in this pass.
- Contract cleanup was prioritized ahead of visual refactoring because lock/copy/attachment drift is a harder production blocker.
