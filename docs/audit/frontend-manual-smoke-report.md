# Frontend Manual Smoke Report

Date: 2026-03-13
Scope: live browser validation of frontend behavior without relying only on repo gates

## Verdict

The frontend is in a strong audit-ready state for a focused SAP frontend review and is now close to a practical freeze state.

Structural debt is no longer the main risk. The main remaining risk is system-side SAP evidence,
not client-side architecture collapse. The main remaining frontend-local risks are low and mostly regression-oriented:

- future visual regressions if shared rail/sticky/theme owners are changed without live validation
- productive-contour differences between local and real SAP Gateway/FLP behavior

## Live Scenarios Verified

### Startup shell

- App starts without the previous split-theme / white-block startup glitch.
- Shell header renders cleanly.
- Theme handoff remains visually stable.
- Shell user/profile rendering is stable on fresh startup contours:
  - a fresh-start render drift was reproduced where the visible shell button still showed `Session profile unavailable` even though `state>/currentUser` and the header control properties were already correct
  - root cause: stale DOM inside `AppShellHeader` / `OverflowToolbar`, not a failed current-user fetch
  - fixed by rerendering the header toolbar host after user-label synchronization in [AppShellHeader.js](C:\Users\lgbtb\Desktop\ui5\app\controls\AppShellHeader.js)
  - verified on fresh startup contour `shellprofilefix3`: visible shell button now shows the resolved user label `Оператор смены - BUKRS 3000: 01, 02, 03, 06`
  - isolated-origin startup remains stable after lazy-pane prewarm was introduced

### Search

- Search route renders.
- Search action rail remains visible and interactive.
- `Create`, `Go`, export-related shell actions, and filter controls render correctly.
- Search page remains the eager critical-path shell as intended by the bundle strategy.
- Fresh startup on isolated origin `:8081` confirms background lazy-pane prewarm is active without introducing startup console errors.
- Deep-scroll sticky behavior remains materially correct:
  - desktop single-column sticky works
  - desktop split-column sticky works
  - mobile/narrow viewport uses compact sticky policy instead of pinning the whole filter shell
  - live deep-scroll DOM measurement on a 101-row result set confirms the sticky stack remains pinned:
    - filter rail top stays at `315.36`
    - action rail top stays at `630.59`
    - table toolbar top stays at `699.91`
  - live route-open timings on the isolated contour confirm non-blocking lazy prewarm:
    - first detail open from search row: about `1084ms`
    - analytics route open from shell: about `407ms`
- Dialog-heavy toolbar flows are stable on a fresh contour:
  - `Sort` opens correctly
  - `Escape` closes `Sort` without leaving an orphaned block layer
  - `Group` opens immediately after `Sort` teardown
  - `Cancel` closes `Group` cleanly
  - export menu opens and closes without interaction lockups

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
  - persisted attachment delete is confirmed live as a separate backend command path:
    - `DELETE AttachmentSet(AttachmentKey='...')`
    - this path is intentionally distinct from unified `SaveChanges` delta semantics
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
- Data-specific access-denied contour is now explicitly verified:
  - opening certain rows from search may render `detailAccessDenied` in the mid pane
  - this currently behaves like a permission/data-specific branch, not a shell/layout crash
  - `Close` on the access-denied state returns cleanly to search and clears the split layout/hash without leaving stale UI

### Analytics

- Direct route `#/analytics` renders successfully.
- Analytics shell, year controls, export/report actions, and breakdown content all render.

## Key Fixes Confirmed By Manual Testing

- Attachment save path hardened:
  - explicit save no longer crashes on staged attachment values that arrive as non-Blob object wrappers
- Detail edit mode / lock state works after `LastChangeSet(RootKey=binary'...')` compatibility fix in mock Gateway.
- Status flow remains `root.status + SaveChanges`, not a separate frontend transport contract.
- Explicit save and autosave now both use the unified delta-first payload shape.
- Lazy detail/analytics pane prewarm is now active as sanctioned background behavior and does not block startup.

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
- Background prewarm timings should be re-measured if pane ownership or startup sequencing changes.

### System-side

- ABAP `AUTHORITY-CHECK`
- ETag / `If-Match`
- productive Gateway / FLP proof
- lock conflict traces
- DDIC / MPC / BO alignment in live SAP system

## Freeze Recommendation

Frontend structure can now be considered frozen for targeted SAP frontend audit purposes and is close to a practical product UX freeze.

Further changes should be limited to:

- business-driven UX fixes
- live route-open timing checks after any shell/startup change
- live-system compatibility fixes
- evidence-driven hardening for SAP Gateway / ABAP productive contour
