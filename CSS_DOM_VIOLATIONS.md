# CSS DOM Violations

## 2026-03-27 Production-Readiness Implementation Delta
- Contract and ownership cleanup was completed ahead of visual cleanup.
- `cmd /c npm run validate:local` now fails only after reaching `sap-internal-css-gate`, which confirms CSS debt is the dominant unresolved release blocker.
- No new `.sap*` selector usage was introduced by this implementation delta.
- Active blocker remains concentrated in:
  - `app/styles/modules/23_dialogs.css`
  - `app/styles/modules/40_page_search.css`
  - `app/styles/modules/41_page_detail.css`
  - `app/styles/modules/controls/30_action_and_shell_buttons.css`
  - `app/styles/modules/controls/32_switch_parity.css`

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
## 2026-03-27 Re-verified Status
- `node scripts/dom-hack-gate.js` still fails. Main remaining offenders:
  - detail: `AttachmentDropZoneRuntime.js`, `DetailControllerRuntime.js`, `DetailInfoCardFactory.js`
  - search: `SearchViewportRuntime.js`, `SearchSelectionRuntime.js`, `SearchReturnRediscoveryRuntime.js`, `SearchStartupRuntime.js`, `SearchViewStateRuntime.js`
  - shell: `ShellLayoutRuntime.js`, `ShellViewportRuntime.js`
- `node scripts/sap-internal-css-gate.js` still fails at scale. Highest-debt modules remain:
  - `app/styles/modules/23_dialogs.css`
  - `app/styles/modules/40_page_search.css`
  - `app/styles/modules/41_page_detail.css`
  - `app/styles/modules/controls/30_action_and_shell_buttons.css`
  - `app/styles/modules/controls/32_switch_parity.css`
  - `app/styles/modules/controls/34_action_priority_and_a11y.css`
