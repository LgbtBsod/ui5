# Sticky Keyboard And Mobile Checklist

Date: 2026-03-14

Purpose: provide a focused audit checklist for `search` and `detail` sticky scenarios, with emphasis on keyboard flow, focus continuity, and mobile viewport behavior.

## Scope

- Search sticky filter workbench
- Search action rail
- Search summary rail
- Search results toolbar anchor flow
- Detail control rail
- Detail section anchor rail
- Detail status rail
- Detail action rail

## Keyboard Checklist

### Search

- Tab order reaches the sticky search filter area before the sticky action rail.
- `Go`, `Clear`, and custom segmented filter controls are reachable without focus traps.
- Pressing the “back to results toolbar” action moves scroll context without losing keyboard focus.
- After search execution, focus returns to a meaningful target:
  - search button
  - results summary
  - first selected row
  - first row when no selection exists
- Opening detail from search results does not strand focus on a detached row element.
- Returning from detail to search restores focus to a meaningful search target:
  - selected row
  - results toolbar
  - smart filter `Go` button
- Overflowed search actions remain keyboard reachable on narrow widths.

### Detail

- Tab order reaches the detail status rail before destructive or workflow actions.
- Edit switch, save, cancel, validate, delete-arm, and delete-confirm controls are all keyboard operable.
- Anchor rail buttons move to the correct section without focus loss.
- Validation summary link focuses the first invalid field reliably.
- Sticky/pinned detail rail does not cover the focused control after section jump.
- When closing detail, focus returns to a stable search-side target.

## Mobile Checklist

### Search

- On compact/mobile widths the search sticky stack does not overlap the shell header.
- Summary rail and results toolbar stack cleanly after the mobile sticky breakpoint.
- Filter card wrapping does not hide `Go` or `Clear`.
- Overflow actions remain available through the toolbar overflow menu.
- No sticky rail covers the first visible results row.

### Detail

- Detail control rail remains visible without consuming excessive viewport height.
- Action row buttons remain reachable without horizontal clipping.
- Section anchor rail wraps predictably and does not hide primary actions.
- Focused inputs in checks/barriers/attachments remain visible when the sticky rail is present.
- Virtual keyboard on mobile does not leave the sticky rail obscuring edited fields.

## Acceptance Criteria

- No focus trap exists in sticky search or detail zones.
- No sticky layer hides the currently focused interactive control.
- No sticky layer obscures primary content after breakpoint transitions.
- Keyboard-only search-to-detail-to-search flow completes without manual mouse correction.
- Mobile-width action overflow remains usable for create, save, validate, delete, and section navigation.

## Evidence To Capture

- One desktop keyboard walkthrough video or annotated screenshot set.
- One mobile-width walkthrough video or annotated screenshot set.
- One defect log for any overlap, focus loss, or unreachable action.
- One accepted proof record mapped to `EV-010`.
