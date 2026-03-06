# Design System

Last updated: 2026-03-04

## Shell

- The app shell owns brand, current workflow context, global actions, settings, notifications entry, and theme switch.
- Canonical implementation: `control/AppShellHeader.js`.
- Global shell actions must remain available in both search-only and split-detail states.
- On desktop widths, shell utility actions expose visible labels; below the shared breakpoint they collapse to icon-first pills without changing behavior.
- Shell chrome uses the same glass language as cards, but with stronger compositional stability and less decorative motion.
- Shell popovers use the same glass family as dialogs and must open from their triggering shell action.
- User shell entry is context-aware: gateway-managed sessions show runtime identity context, while standalone/test sessions expose the test-user switch action.
- The user popover primary action is runtime-driven: `Refresh runtime context` for gateway-managed sessions, `Change test user` for standalone/test sessions.
- Sticky detail controls respect shell clearance instead of pinning to raw viewport `0`.
- Sticky offsets consume the measured shell token rather than a guessed fixed header height.

## Page Composition

- Search page pattern: command-center top deck, filter workbench, then results workstation.
- Detail page pattern: hero summary board, workflow strip, sticky control rail, then editable sections.
- Do not collapse these page-level compositions back into generic card stacks unless the workflow is simplified globally.

## Buttons

| Pattern | Use | Rules |
| --- | --- | --- |
| Primary pill button | Create, Save, commit actions | Full pill radius, strongest contrast, one per action group |
| Ghost pill button | Secondary commands in rails | Border or soft fill, same height as primary |
| Transparent icon button | Tertiary actions | Only inside dense rails or tables; always with tooltip |
| Table row action button | Row delete/download/open actions | Compact text-plus-icon button, labeled `Actions` column, danger tint for destructive actions |
| Destructive button | Delete and irreversible actions | Red semantic treatment, never the default emphasis |

- In the detail control rail, `Save`, `Cancel`, `Close`, and `Validate` should prefer visible text labels over icon-only presentation.
- Shell utility buttons reuse the same pill family, but with lighter emphasis than workflow-primary actions.
- Checklist deletion is a detail-context action and should sit in the edit control rail, not in the search action toolbar.

## Status Chips

- `ObjectStatus` is the canonical status primitive.
- Chips are pill-shaped, compact, and color by semantic state.
- Search summary chips, detail workflow chips, and toolbar status chips share the same padding and border treatment.
- When the product needs to expose many checklist facts at once, use summary cards (`uxStatChip`) instead of continuing to add more inline pills.

## Forms

- Card-level forms are grouped by semantic section, not arbitrary field count.
- Labels stay close to inputs; helper text sits below the field.
- Required-field and validation errors use inline field state first, then a section or page summary for grouped failures.
- Suggest, select, and value-help controls must visually align with text inputs.
- Search workbench rails and controls consume the shared `--theme-workbench-*` token family rather than page-local colors or borders.
- Segmented filters use a single shared contour, no inner borders, and semantic active fills for accent/danger/success states.

## Tables

### Search Table

- Optimized for scan, selection, and export.
- Summary and filter context stay above the table.
- Empty state explains the next action, not just the absence of rows.

### Detail Table

- Optimized for row editing, add/delete, and semantic results.
- Editing controls stay inside cells, with row actions at the edge.
- Row action cells are explicit `Actions` columns with compact labeled buttons instead of tooltip-only icon affordances.
- `Actions` headers use the shared badge treatment in both `sap.ui.table` and `sap.m.Table`.
- If a row action already exposes visible text, do not rely on tooltip-only meaning for the action label.
- Per-theme table density comes from `--theme-table-row-height`, not per-screen row overrides.
- Mobile mode uses list/table fallback with the same content order.
- Table wrappers and smart-toolbars must wrap, shrink, and clamp inner widths before horizontal overflow is introduced.

## Dialogs

- Default dialog class: `glassDialog`.
- Dialog body uses `dialogContentShell`.
- Data-heavy dialogs use `tableDialogBody` or `valueHelpDialogBody`.
- Footer buttons follow clear primary and secondary hierarchy.
- Close-only dialogs use a single close action.
- Value-help tables keep a stable fill-container layout and debounce live search updates to avoid visual jitter.

## Popovers and Action Sheets

- Popovers inherit the same radius, border, and surface model as dialogs.
- Use popovers for lightweight status, settings, and help.
- Use action sheets for user or contextual command lists only when a menu is the clearest affordance.

## Toasts and Notifications

- Toasts confirm lightweight success only.
- Error and warning states should surface inline or in a product-styled dialog.
- Notification center content must be actionable, not just decorative counts.
- Effect-layer feedback must route through `glassDialog` and `glassToast` styling, not raw UI5 chrome.

## Loading, Empty, and Error States

- Skeletons mirror final layout geometry.
- Empty states include a title, short explanation, and next action when possible.
- Errors are factual and operational. Avoid generic failure copy.

## Accessibility Baseline

- Keyboard focus must be visible on all shell, table, and dialog actions.
- Icon-only controls require tooltip and accessible name.
- Dialog focus returns to the triggering control after close.
- Search-led value helps focus the search field first when opened from keyboard or pointer.
- Rail switches must respond to both `Space` and `Enter`, not only pointer interaction.
- Theme contrast must preserve readability for labels, chips, and rails in both modes.
