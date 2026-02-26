# UX Design Governance Checklist (C1)

## Required state coverage (critical screens)
- Search: loading, empty, error, conflict, permission.
- Detail: loading, empty-subsection, error, conflict, permission.
- Unsaved-close flow: prompt/cancel/discard/save.

## Consistency rules
- Spacing: 4/8/12/16/24 rhythm only.
- Typography: SAP 72 stack, H4/H6 hierarchy for cards/sections.
- Components: `MessageStrip` for state messaging, `ObjectStatus` for live status, `Button` emphasis capped to one primary per rail.
- Themes: visual parity must be checked on Horizon Morning (`sap_fiori_3`) and Horizon Night (`sap_fiori_3_dark`).

## PR evidence (mandatory)
- Attach completed checklist artifact (`docs/ux/baselines/state-coverage.json`).
- Attach Morning + Night screenshots for key state snapshots (auto-generate via `python scripts/generate-ux-baseline-screenshots.py`).
- Attach gate outputs: state coverage, a11y, SLO, message taxonomy.


## Browser validation matrix (confirmed)
- Required: Microsoft Edge, Google Chrome, Yandex Browser, Firefox.
- Single visual specification shared across desktop/mobile for now (no separate mobile variant yet).
