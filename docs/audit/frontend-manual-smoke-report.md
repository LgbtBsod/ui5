# Frontend Manual Smoke Report

Date: 2026-03-13
Scope: live browser validation of frontend behavior without relying only on repo gates

## Verdict

The frontend is in a strong audit-ready state for a focused SAP frontend review, but not in a zero-risk freeze state yet.

Structural debt is no longer the main risk. The main remaining risk is system-side SAP evidence,
not client-side architecture collapse. The main remaining frontend-local risks are:

- inconsistent shell user/profile rendering on some fresh startup contours
- a dev-runtime stale `?eval` create-route error that still appears in the browser console even after code-side guard fixes

## Live Scenarios Verified

### Startup shell

- App starts without the previous split-theme / white-block startup glitch.
- Shell header renders cleanly.
- Theme handoff remains visually stable.
- Shell user/profile rendering is not yet consistently stable across all fresh route contours:
  - some live runs still show the fallback `Session profile unavailable`
  - this is no longer treated as a structural architecture issue, but it is still a live-product polish risk

### Search

- Search route renders.
- Search action rail remains visible and interactive.
- `Create`, `Go`, export-related shell actions, and filter controls render correctly.
- Search page remains the eager critical-path shell as intended by the bundle strategy.
- Deep-scroll sticky behavior remains materially correct:
  - desktop single-column sticky works
  - desktop split-column sticky works
  - mobile/narrow viewport uses compact sticky policy instead of pinning the whole filter shell
  - live deep-scroll DOM measurement on a 101-row result set confirms the sticky stack remains pinned:
    - filter rail top stays at `315.36`
    - action rail top stays at `630.59`
    - table toolbar top stays at `699.91`

### Detail route

- Direct route `#/checklist/A344649AEDF34458B307C669ADC35626` renders the middle-column detail page.
- Entering edit mode acquires the lock and updates runtime state:
  - heartbeat switches to locked-active state
  - autosave switches to waiting / saved state
  - Save / Delete / Validate / status buttons appear
- Turning edit mode off now stops edit-lifecycle timers correctly:
  - heartbeat manager stops
  - autosave manager stops
  - no false `lockReleaseFailed` warning is shown when leaving detail after manually switching back to `READ`
- Status change remains ordinary save semantics:
  - clicking `Registered` on incomplete data triggers validation instead of a dedicated transport command
  - required field hints appear on mandatory cards and fields
- Editing a field and blurring it triggers autosave state transition to `autosaveSaved`.
- Attachment create/delete live flow now works through the unified save contract:
  - staged attachment save no longer crashes on non-Blob `_file` values
  - live `SaveChanges` trace confirms attachment create is emitted through unified delta with `attachments[].edit_mode = C`
  - deleting a temporary staged attachment without a persisted server key remains a local-only action until a real attachment key exists; this is currently treated as accepted semantics, not a backend delete regression
- Explicit save vs autosave wire trace is confirmed live:
  - `SaveChanges` sends root-only unified delta when only root data changes
  - `AutoSave` sends the same unified delta shape through `AutoSave`
  - payload shape observed live:
    - `root`
    - `checks`
    - `barriers`
    - `participants`
    - `attachments`
    - `client_version`
- Create-draft autosave guard is confirmed live:
  - before the first successful create-save, no `AutoSave` request is emitted for `#/checklist/__CREATE`
  - autosave only becomes active after a real object id exists and edit lock is active
- Create-route navigation is now synchronized correctly:
  - after the first create-save, browser hash is replaced with the real checklist id
  - after switching edit mode off, heartbeat and autosave managers stop
  - after closing the card, browser hash returns to the search route and detail state is cleared cleanly

### Analytics

- Direct route `#/analytics` renders successfully.
- Analytics shell, year controls, export/report actions, and breakdown content all render.

## Key Fixes Confirmed By Manual Testing

- Attachment save path hardened:
  - explicit save no longer crashes on staged attachment values that arrive as non-Blob object wrappers
- Detail edit mode / lock state works after `LastChangeSet(RootKey=binary'...')` compatibility fix in mock Gateway.
- Status flow remains `root.status + SaveChanges`, not a separate frontend transport contract.
- Explicit save and autosave now both use the unified delta-first payload shape.

## Remaining Accepted Frontend Debt

- `SearchControllerBehavior.js` is still a sanctioned top-level orchestrator, though much smaller than before.
- `Component.js` remains a sanctioned entry shell, not a pure empty wrapper.
- `AnalyticsPayloadNormalizer.js` remains a sanctioned composition owner.
- `GatewayClient.js` remains a sanctioned public transport shell.

These are accepted orchestrators, not uncontrolled legacy blobs.

## Remaining Risks

### Frontend-local

- Sticky behavior is now materially better and works in live scenarios, but future changes to search rail / results shell should always be re-validated visually.
- Autosave interaction depends on real change/blur behavior; synthetic fill without blur is not a reliable user-equivalent action.
- Fresh create-route startup in dev runtime can still show stale console error:
  - `Cannot read properties of undefined (reading 'then')`
  - code-side guards were added, but `?eval` browser caching still showed the stale path in this session
- Shell user/profile display is still inconsistent across fresh route contours and should not yet be treated as fully closed.

### System-side

- ABAP `AUTHORITY-CHECK`
- ETag / `If-Match`
- productive Gateway / FLP proof
- lock conflict traces
- DDIC / MPC / BO alignment in live SAP system

## Freeze Recommendation

Frontend structure can now be considered frozen for targeted SAP frontend audit purposes, but not yet for zero-risk product UX freeze.

Further changes should be limited to:

- business-driven UX fixes
- shell user/profile rendering stabilization
- live create-route replay validation in a fresh browser/runtime without stale `?eval` cache
- live-system compatibility fixes
- evidence-driven hardening for SAP Gateway / ABAP productive contour
