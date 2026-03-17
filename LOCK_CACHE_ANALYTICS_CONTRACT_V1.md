# LOCK / CACHE / ANALYTICS CONTRACT V1

This file is the canonical implementation contract for the current SAP UI5 application.
Use it as the source of truth for frontend runtime, mock backend, and ABAP backend.

---

## 1. Lock ownership and TTL

### Canonical rules
- Backend lock TTL = `600 sec`.
- Lock validity is determined by `last_refresh_at`, not `created_at`.
- A lock is valid if:
  - `server_now - last_refresh_at < 600 sec`
- Cleanup job every `15 min` is **cleanup only**.
- Cleanup job does **not** define runtime truth.
- Backend must evaluate lock validity directly during:
  - lock acquire
  - lock refresh / heartbeat
  - lock status
  - save
  - autosave
  - release / takeover validation when applicable

### Owner identity
- Owner identity is at least:
  - `owner_user_id`
  - `owner_session_guid`
- Different tabs must be treated as different sessions.
- Same-user takeover is a separate scenario and must not be merged with “different user stole lock”.

### Required backend codes
Backend must distinguish at least:
- `LOCK_OK`
- `LOCK_EXPIRED`
- `LOCK_STOLEN`
- `LOCK_NOT_OWNED_BY_SESSION`
- `LOCK_MISSING`
- `PERMISSION_DENIED`

### Required backend response fields
For lock status / save / autosave / refresh responses, backend should return:
- `ok`
- `code`
- `lock_expires_at`
- `server_now`
- `lock_refreshed` (for save/autosave/refresh flows)
- `owner_session_match` where relevant

---

## 2. Edit session runtime

### Canonical runtime constants
- `backendLockTtlSec = 600`
- `heartbeatMs = 270000` (4.5 min)
- `lockStatusMs = 60000` (60 sec)
- `idleMs = 570000` (9.5 min)
- `autoSaveIntervalMs = 150000` (2.5 min)
- `lockRefreshCooldownMs = 150000` (2.5 min)
- `analyticsRefreshMs = 900000` (15 min)
- `cacheToleranceMs = 5500` (5.5 sec)

### Profile model
Introduce exactly two timing profiles:
- `production`
- `test`

Rules:
- profile selection is from environment/config only
- no end-user UI toggle
- frontend runtime settings, mock backend settings, and ABAP defaults must stay aligned

---

## 3. Inactivity model

### What counts as activity
Activity must be based only on real user interaction.
Allowed activity events:
- `click`
- `keydown`
- `input`
- `change`
- `scroll`
- `touchstart`

Rules:
- `scroll` listener should be passive
- `mousemove` must not be used as a keep-alive event in v1
- background data updates do not count as activity
- heartbeat/autosave/lock status do not count as activity

### Inactivity timeout outcome
If there is no activity for `9.5 min` while in edit mode:
- force readonly
- stop heartbeat
- stop lock status poll
- stop autosave
- clear all edit-session timers
- best-effort release lock
- reset runtime to clean read-only state
- show message to the user

### Release behavior
- Frontend release is best-effort.
- Backend must tolerate duplicate or late release.
- If release fails, TTL still expires the lock later.

---

## 4. Heartbeat, cooldown, and status poll

### Heartbeat
- Heartbeat exists only in healthy edit mode.
- Nominal interval = `4.5 min`.
- Heartbeat is a lock refresh mechanism.
- Heartbeat remains mandatory in edit mode.

### Lock status poll
- Lock status poll interval = `60 sec`.
- Lock status poll is read-only.
- Lock status poll never refreshes the lock.
- Lock status poll remains enabled in edit mode because it detects lock loss earlier than heartbeat.

### Refresh cooldown
- Cooldown duration = `2.5 min`.
- Cooldown applies only after an **actual backend-confirmed lock refresh**.
- Save success without refresh must not start cooldown.
- Cooldown is only an anti-spam mechanism for refresh-lock traffic.
- Cooldown must not suppress lock validation on save/autosave.

### Coordinator requirement
Use one edit-session coordinator as the source of truth for:
- heartbeat
- lock status poll
- autosave scheduling
- inactivity timeout
- refresh cooldown state
- clean teardown on exit / lock loss / inactivity

---

## 5. Save and autosave contract

### Backend validation for save/autosave
Backend must validate:
- lock valid
- owner session match
- TTL not expired
- permissions valid

### Manual save
Manual save must:
- show `Saving…`
- prevent duplicate concurrent save
- not race with autosave
- set canonical persistence state

On success:
- show `Saved`
- set dirty = false
- update last saved timestamp
- update lock data if returned
- apply cooldown only if `lock_refreshed = true`

On generic failure:
- show `Not saved`
- keep dirty = true
- allow retry

On lock-related failure:
- show lock-lost message
- immediately transition to `lock-lost / readonly`
- stop autosave
- stop edit timers as needed
- do not wait for heartbeat or lock status to catch up

### Autosave
Autosave runs only if:
- edit mode is active
- dirty = true
- no write is in flight
- screen is not already in lock-lost state

Autosave UX:
- `Autosaving…`
- `Saved`
- `Not saved`

Autosave lock-related failure:
- immediate readonly transition
- no endless retry loop pretending edit mode is still healthy

### Dirty data on lock loss
V1 decision:
- do not attempt to push unsaved local draft after lock loss
- do not keep the form editable
- reset/reload from server snapshot
- show a clear message that unsaved changes were not applied

### Successful save followed by later lock loss
If save succeeded and lock is lost afterwards:
- save remains successful
- editing rights are lost afterwards
- UI must leave edit mode immediately

---

## 6. Cache contract

### Chosen model
Use the simple model only:
- aggregate last-change stamp from backend
- compare with cached snapshot stamp
- invalidate when stamp difference exceeds `cacheToleranceMs = 5500`

### Explicit non-goals for v1
Do not pretend this is a full TTL cache with:
- fresh
- stale_ok
- expired

### Practical wording
This is a **stamp-based consistency cache**, not a full freshness-state cache.

---

## 7. Analytics lifecycle contract

### Full analytics page
When entering the full analytics route:
- load analytics immediately
- start route-scoped timer = `15 min`

When leaving the full analytics route or controller exits:
- clear timer

When re-entering:
- load fresh analytics again
- restart timer

### Search analytics rail
May keep its own analytics refresh lifecycle if already implemented, but it must not conflict with full page lifecycle.

### Localization
Analytics UI and export must not hardcode:
- `Month`
- `Selected year`
- `Compare year`
- `Jan..Dec`

All must use i18n keys.

---

## 8. Observability and diagnostics

Strongly recommended runtime/backend fields:
- `server_now`
- `request_id`
- `lock_expires_at`
- `lock_refreshed`

Also log reasons for lock loss so production incidents can be diagnosed.

---

## 9. V1 implementation priority

### P0
- align TTL to 600 sec everywhere
- use `last_refresh_at`
- implement immediate readonly on save/autosave lock failure
- implement inactivity teardown
- implement hard reset/reload after lock loss

### P1
- activity monitor event allowlist
- lock status payload with `code + lock_expires_at`
- same-user takeover flow
- cooldown only after actual refresh
- full analytics page timer
- centralized edit-session coordinator

### P2
- `server_now`
- `request_id`
- explicit lock-loss logging
