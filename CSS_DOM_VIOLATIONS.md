# CSS DOM Violations

## Closed In This Pass
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
