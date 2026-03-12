# UI Governance Layer (2026-03-12)

## Purpose

This project now uses a small named UI governance layer so `search`, `detail`, and `analytics`
share the same behavioral and visual contracts instead of evolving independent local patterns.

The layer is intentionally UI-only. It does not change:

- OData/Gateway paths
- controller actions
- lock/edit/autosave business invariants
- backend integration contracts

## Canonical contracts

### Toolbar surfaces

- `workbenchToolbarSurface`
  - shared shell for operational toolbars
  - used by search and analytics workbench-level toolbars
- `sectionToolbarSurface`
  - shared shell for section-local toolbars
  - used by detail section and attachment toolbars
- `actionToolbarSurface`
  - shared shell for action-heavy rails
  - used by search action rail

### Action priority

- `actionPriorityPrimary`
  - use for the main action in a toolbar or rail
  - examples: create, save, refresh
- `actionPrioritySecondary`
  - use for supporting actions
  - examples: expand, copy link, sort, group, toggle section
- `actionPriorityDanger`
  - use only for destructive or confirm-delete actions

### Status chip semantics

- `statusChipSemantic`
  - common status-chip shell for `ObjectStatus`-based pills
  - use for refresh state, selection state, mode/state chips, period/update chips

### State surfaces

- `workflowMessageSurface`
  - canonical wrapper for `MessageStrip` surfaces
- `workflowBusySurface`
  - canonical wrapper for busy cards/overlays
- `workflowEmptyStateCard`
  - canonical wrapper for empty-state cards

## Usage rule

When adding a new toolbar, button rail, chip, message strip, busy block, or empty state:

1. start from these contract classes
2. add local page classes only for layout-specific tuning
3. do not introduce a page-private visual language unless the behavior is truly unique

## Current references

- `app/view/Search.view.xml`
- `app/view/Detail.view.xml`
- `app/view/fragment/DetailControlRail.fragment.xml`
- `app/view/Analytics.view.xml`
- `app/view/fragment/WorkflowAnalyticsBreakdowns.fragment.xml`
- `app/css/modules/21_controls.css`
- `app/css/modules/42_page_analytics.css`

## Architectural intent

This keeps the product aligned with SAP/UI5 enterprise delivery expectations:

- consistent operator ergonomics
- lower design drift across modules
- easier review of new UI additions
- stronger licensing/product-readiness posture
