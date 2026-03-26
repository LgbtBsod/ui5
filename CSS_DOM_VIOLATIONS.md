# CSS / DOM Violations

## 2026-03-27 Final Pass Status

### DOM
- `dom-hack-gate` passes.
- No additional DOM-dependent owner was introduced in this pass.
- Main browser-boundary owners remain:
  - `app/service/framework/ThemeDomRuntime.js`
  - `app/service/features/shell/runtime/AppShellDomRuntime.js`
  - existing allowlisted search/detail/shell runtimes already tracked by `dom-hack-gate`
- DOM quarantine is now owned by [`dom-hack-allowlist.json`](/Users/lgbtb/Desktop/ui5/scripts/dom-hack-allowlist.json), not a hardcoded JS array.
- `dom-hack-gate` now fails if the DOM allowlist grows or if a quarantine reason becomes vague.
- route binding behavior was moved out of deleted thin wrapper `ControllerRouteRuntime` and into route-owning controller behaviors

### CSS
- `sap-internal-css-gate` passes.
- Remaining SAP-internal selector usage is not treated as resolved; it is quarantined through [`sap-internal-css-allowlist.json`](/Users/lgbtb/Desktop/ui5/scripts/sap-internal-css-allowlist.json).

## Documented Legacy CSS Whitelist
- `app/styles/modules/22_skeleton.css`
  - skeleton placeholder parity still depends on SAP internals
- `app/styles/modules/23_dialogs.css`
  - dialog/popover chrome still depends on SAP dialog structure
- `app/styles/modules/40_page_search.css`
  - search workbench sticky rails and smart controls still depend on SAP internals
- `app/styles/modules/41_page_detail.css`
  - detail workspace card/form polish still depends on SAP internals
- `app/styles/modules/42_page_analytics.css`
  - analytics host layout still bridges SAP internals
- `app/styles/modules/90_ui5_overrides.css`
  - central quarantine file for unavoidable UI5 overrides
- `app/styles/modules/controls/22_feedback_and_status.css`
  - feedback strips/status controls still depend on SAP internals
- `app/styles/modules/controls/23_shell_controls.css`
  - shell controls still depend on SAP renderer structure
- `app/styles/modules/controls/25_table_actions.css`
  - table action affordances still depend on SAP table internals
- `app/styles/modules/controls/26_controls.css`
  - shared control styling still depends on SAP form-control internals
- `app/styles/modules/controls/27_chips_and_kpis.css`
  - KPI/object-status styling still depends on SAP internals
- `app/styles/modules/controls/29_workflow_surfaces.css`
  - workflow surface composition still bridges SAP internals
- `app/styles/modules/controls/30_action_and_shell_buttons.css`
  - button theming still depends on SAP button internals
- `app/styles/modules/controls/32_switch_parity.css`
  - switch parity styling still depends on SAP switch internals

## Rules
- New internal `.sap*` selector usage is forbidden outside the whitelist.
- Each whitelist entry must keep a concrete reason in `sap-internal-css-allowlist.json`.
- Each DOM allowlist entry must keep a concrete reason in `dom-hack-allowlist.json`.
- CSS and DOM quarantine lists are now size-capped to prevent silent debt growth.
- Removing a selector from a file requires removing or updating its whitelist entry, otherwise the gate fails on stale allowlist debt.
