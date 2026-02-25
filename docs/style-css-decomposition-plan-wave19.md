# CSS decomposition plan for `css/style.css` (Wave 19)

## Goal
Keep a **single style entrypoint** (`css/style.css`) while splitting maintenance responsibilities by semantic sections.

## Current constraints
- `manifest.json` must keep one CSS include: `css/style.css`.
- No functional UI regressions during split.
- Keep glass/theme tokens centralized.

## Proposed semantic sections (inside `style.css`)
1. **tokens-and-theme-modes**
   - `:root` tokens, light/dark overrides.
2. **layout-shell-and-page-surfaces**
   - shell/background/full-height containers.
3. **card-toolbar-button-primitives**
   - glass cards, toolbars, button base interactions.
4. **forms-filters-smart-controls**
   - input/select/filter bar/smart table controls.
5. **search-kpi-workflow-blocks**
   - KPI widgets, workflow analytics panels, summary rows.
6. **detail-card-rail-and-dialogs**
   - detail page cards, rail, expanded dialog styles.
7. **responsive-and-utility-overrides**
   - media queries, targeted overrides, migration blocks.

## Implementation approach
1. Insert clear section headers in `css/style.css`.
2. Move rules under matching sections without selector/value change.
3. Keep legacy migration block at bottom until Wave 20 cleanup.
4. Validate with smoke + browser screenshot after each section pass.

## Validation checklist
- `node scripts/unit-smoke.js`
- open Search + Detail pages and compare baseline visuals
- ensure only `css/style.css` remains in resource wiring

## Wave 20 follow-up
- Convert the temporary migration block into normalized token section.
- Remove duplicate token declarations (`--radius-*`, `--transition`) by keeping one source of truth.
