# Production cleanup audit

## Clean runtime state

- Runtime CSS entrypoint is now `css/style.css` via `manifest.json`.
- `style-reset-minimal.css` is no longer the intended runtime layer.
- The old bridge layer is already gone.
- `css/archive/snapshot-2026-03-06` is legacy backup only, not runtime code.

## What was actually broken

- The project drifted into a dual-style runtime:
  - `style.css` existed as the nominal modular stack.
  - `style-reset-minimal.css` became a second live stack with copied rules.
- `style-reset-minimal.css` also imported archived dialog CSS, which made the runtime depend on a backup snapshot instead of the active module tree.
- Detail top-zone rendering had conflicting repeated overrides in `41_page_detail.css`, causing the checklist control rail to collapse and paint phantom stripes over the first info-card row.
- Search results table used sticky header-cell behavior that produced overlap/visual corruption around the no-data state.

## What was repaired

- Switched runtime back to the normal modular entrypoint: `css/style.css`.
- Verified the switch with live browser crawl:
  - `docs/artifacts/stylecss-clean-switch-r1`
  - `docs/artifacts/stylecss-clean-switch-r7`
- Removed the search header-cell sticky conflict by simplifying table header behavior in `css/modules/40_page_search.css`.
- Normalized the detail control rail geometry in `css/modules/41_page_detail.css` so internal containers no longer collapse to zero-height.

## Legacy and cleanup targets

Safe to remove from the repository:

- `css/style-reset-minimal.css`
- `css/archive/snapshot-2026-03-06/`
- generated visual runs under `docs/artifacts/`
- runtime logs/pids under `docs/runtime/`

These are not source-of-truth implementation anymore.

## Remaining residual UI issue

- The detail top control rail still reserves layout imperfectly in the desktop detail screen.
- This is no longer a runtime-stack problem; it is a local detail-layout issue inside `css/modules/41_page_detail.css` and the `DetailControlRail` view structure.

## Target architecture

The clean target remains:

- `css/style.css` as the only runtime CSS entrypoint
- modular CSS only:
  - `10_base.css`
  - `20_surface.css`
  - `21_controls.css`
  - `22_skeleton.css`
  - `23_dialogs.css`
  - `40_page_search.css`
  - `41_page_detail.css`
  - `90_ui5_patches.css`
- no archive imports
- no recovery bridge
- no parallel minimal runtime stack
