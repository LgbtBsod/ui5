# CSS Internal Selector Governance

## Scope
This document defines which residual UI5 internal CSS selectors are still tolerated in the project after the remediation pass, why they remain, and what is required to remove them safely without breaking UI5 1.71 behavior.

The target rule is simple:

- Prefer app-owned host classes first.
- Use internal `.sapM*` / `.sapUi*` selectors only when UI5 renderer DOM is the only stable extension point left.
- Every remaining internal selector must belong to one of the buckets below.

## Allowed Exceptions

These selectors are currently acceptable because they style renderer internals that do not have an equivalent app-owned host node we can target without replacing the control.

### 1. Control internals that expose text/icon wrappers only through renderer DOM

- `sapMBtnInner`
- `sapMBtnContent`
- `sapMBtnIcon`
- `sapMBtnIconLeft`
- `sapMObjStatusText`
- `sapUiIcon`
- `sapMText`
- `sapMTitle`
- `sapMLabel`

Allowed only when scoped to app-owned blocks such as:

- `.appShellHeader`
- `.brandActionBtn`
- `.searchToolbarNavBtn`
- `.actionPriority*`
- `.detailRailStatusChip`
- `.detailWorkflowChip`
- `.shellPopover*`

Reason:
UI5 1.71 button, status, and text controls do not expose enough hook points to fully skin inner text/icon layout from XML alone.

### 2. Overflow toolbar internals

- `sapMOTBOverflowButton`
- `sapMOverflowToolbarMenuButton`
- `sapMTBSpacer`

Allowed only inside:

- search action rails
- detail control rows
- detail section toolbars
- analytics toolbars
- shell header

Reason:
Overflow button and spacer layout are still controlled by renderer-generated nodes. Removing these selectors would require replacing `OverflowToolbar` usage or adding custom wrapper controls.

### 3. Panel, table, and tabbar renderer DOM

- `sapMPanel*`
- `sapMList*`
- `sapMListTbl*`
- `sapUiTable*`
- `sapMITB*`
- `sapMVIZVIZFrame`

Allowed only inside:

- `.analyticsPanelSurface`
- `.analyticsChartPanel`
- `.analyticsMatrixTable`
- `.analyticsTabBar`
- `.searchResultsTable`
- `.detailAttachmentTable`

Reason:
Analytics and table presentation still depends on `sap.m.Panel`, `sap.m.Table`, `sap.ui.table.*`, and `sap.m.IconTabBar` renderer markup. A full removal requires control swaps or additional wrapper composition.

### 4. Switch internals

- `sapMSwt*`

Allowed only for:

- `.appAccentSwitch`
- `.searchModeToggleSwitch`
- `.detailResultSwitch`
- `.shellThemeSwitch`
- `.shellSettingsSwitch`

Reason:
UI5 switch text/handle/track in 1.71 must still be styled via renderer classes.

## Temporary Exceptions

These selectors are tolerated for now but should be removed only through targeted feature work, not broad CSS sweeps.

### 1. Control skin aggregation

File: `app/styles/modules/controls/26_control_skin.css`

- `sapMInputBaseContentWrapper`
- `sapMInputBaseInner`
- `sapMSlt`
- `sapMSltLabel`
- `sapMSegB`
- `sapMSegBBtnInner`
- `sapMSwt*`

Removal path:

- Keep these selectors scoped to app-owned hosts only: search filter cards, search results table, detail section cards, analytics toolbar inputs, and named switch hosts.
- Remove them only when those controls are wrapped by app-owned composition blocks or replaced by custom input/select/toggle wrappers.

### 2. Detail attachments and history timeline

File: `app/styles/modules/detail/44_detail_attachments.css`

- timeline internals: `sapMTLI*`
- attachment object text internals: `sapMObjectIdentifierText`
- a few status/text renderer hooks: `sapMObjStatus`, `sapMObjStatusText`

Removal path:

- Add wrapper classes around timeline item content if timeline remains custom-skinned.
- Replace remaining `ObjectIdentifier/ObjectStatus` typography overrides with app-owned wrapper blocks only if the current visual delta still matters.

### 3. Detail rail and object shell layout

Files:

- `app/styles/modules/detail/46_detail_rail_shell.css`
- `app/styles/modules/detail/48_detail_rail_layout.css`
- `app/styles/modules/detail/50_detail_object_shell.css`

- `sapMSwt`
- `sapMObjStatusText`

Removal path:

- Replace remaining switch/status text hooks only through control replacement or additional wrapper composition.
- Keep `ObjectStatus` text and switch internals as accepted renderer hooks unless the controls are replaced.

## Replacement Candidates

These areas would benefit from targeted XML hook classes or control replacement work.

### 1. Search toolbar

Needed XML hooks:

- class for backend-top input host
- class for max-rows input host
- class for mode-switch host
- class for overflow menu trigger host

Primary file:

- `app/views/fragment/SearchActionRail.fragment.xml`

Status:

- Implemented for input hosts, switch host, spacers, and status chips.
- Remaining internal hooks are limited to overflow button internals and switch renderer nodes.

### 2. Detail attachments toolbar and table

Needed XML hooks:

- class per toolbar child block
- class for category select host
- class for history timeline content shell
- class for attachment name / description / metadata text nodes

Primary files:

- `app/views/fragment/DetailAttachmentsSection.fragment.xml`
- `app/views/fragment/DetailAttachmentsBody.fragment.xml`

Status:

- Implemented for attachment rows, action cell, category cell, size cell, changed cell, and picker meta blocks.
- Timeline renderer DOM remains temporary exception.

### 3. Detail status chips

Needed hook classes:

- `chipStateSuccess`
- `chipStateWarning`
- `chipStateError`
- `chipStateInfo`
- `chipStateDraft`
- `chipStateLocked`

Primary files:

- `app/views/fragment/DetailControlStatusRow.fragment.xml`
- `app/views/fragment/LockSwitchStatus.fragment.xml`
- formatter/view-state sources that compute chip semantics

Status:

- Implemented through runtime-applied `chipState*` classes in `StatusChipClassRuntime`.
- UI5 semantic classes are no longer accepted as temporary chip styling contract.

### 4. Analytics panels and tabbar

Potential control replacements:

- replace part of `sap.m.Panel` skinning with wrapper `VBox` + title block where possible
- keep `sap.viz` and `IconTabBar` internals only where the renderer is unavoidable

Primary files:

- `app/views/fragment/WorkflowAnalyticsBuilder.fragment.xml`
- `app/views/fragment/WorkflowAnalyticsTrends.fragment.xml`
- `app/views/fragment/WorkflowAnalyticsBreakdowns.fragment.xml`

Status:

- Toolbar labels/selects/inputs/spacers/status chips now have app-owned analytics hook classes.
- Remaining internal selectors in analytics are now only panel/tabbar/table/viz renderer hooks.

## High-Risk Files Still Requiring Review

- `app/styles/modules/controls/26_control_skin.css`
- `app/styles/modules/detail/44_detail_attachments.css`
- `app/styles/modules/analytics/43_analytics_controls.css`
- `app/styles/modules/analytics/44_analytics_panels.css`
- `app/styles/modules/detail/46_detail_rail_shell.css`
- `app/styles/modules/detail/48_detail_rail_layout.css`
- `app/styles/modules/detail/50_detail_object_shell.css`

These files are no longer broad framework-wide override files, but they still contain the highest concentration of scoped renderer-hook selectors.

## Forbidden From Now On

Do not add any new selectors using:

- global `.sapM*` / `.sapUi*` without app-owned host scope
- `sapMTBNewFlex`
- `sapMBarChild`
- generic `sapMTBSpacer` in search/detail rails when app-owned spacer hooks exist
- parent-chain selectors that depend on anonymous flex wrappers
- `[style*=...]`
- `:has(...)`
- generic semantic styling through `CustomData role=*`

## Enforcement Rule

Any new internal selector must meet all of the following:

1. Be scoped to an app-owned host class.
2. Be documented in this file or replace an already documented exception.
3. Have a clear removal path if it is not in the `Allowed Exceptions` bucket.
