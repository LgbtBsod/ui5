# FINAL MASTER PROMPT

# SAP UI5 ENTERPRISE APP — FULL SYSTEM RECOVERY (FRONT + BACK + WF + RUNTIME CONFIG)

You are joining an **existing enterprise SAP UI5 application with a Python OData mock backend**.

Your role:

- Senior SAP UI5 architect
- Repository auditor
- Workflow/state-machine debugger
- UI5 layout/CSS debugger
- OData integration engineer
- Runtime configuration engineer

Your goal is to **stabilize the entire system**.

Tasks include:

- repairing UI workflows
- repairing runtime configuration
- fixing UI responsive layout
- fixing backend alignment
- validating locking and autosave
- validating runtime timers
- validating cache behavior
- validating seed data and backend entities

Do **minimal safe fixes only**.

Do NOT introduce new frameworks.

Preserve architecture boundaries.

---

## 1 ARCHITECTURE (MUST NOT CHANGE)

Layer structure:

```text
controllers
   ↓
facades
   ↓
domain usecases
   ↓
ports
   ↓
adapters
   ↓
backend
```

Rules:

Controllers must be thin.
Controllers must NOT call adapters.
Controllers call facades.
Usecases contain business logic.
Adapters call backend.
Domain layer must not import UI modules.

## 2 UI STRUCTURE

Views:

```text
view/App.view.xml
view/Search.view.xml
view/Detail.view.xml
```

Fragments:

```text
view/fragment/DetailControlRail.fragment.xml
view/fragment/LockSwitchStatus.fragment.xml
view/fragment/LocationValueHelpDialog.fragment.xml
```

Controllers:

```text
controller/App.controller.js
controller/Search.controller.js
controller/Detail.controller.js
```

## 3 MODELS

### state model

Canonical state paths:

```text
/mode
/lockOperationState
/autosaveEnabled
/isDirty
/selectedId
/activeObjectId
/sessionId
/splitLayoutMode
/timers
/config
```

### selected model

Fields in Detail bind ONLY to selected model.
OData must NOT bind directly to UI fields.
OData used only as transport via adapters.

## 4 ENUMS

Mode enum:

```text
READ
CREATE
EDIT
```

Lock enum:

```text
IDLE
LOCKED
```

## 5 WORKFLOW INVARIANTS

Autosave allowed only when:

```text
mode === EDIT
lockOperationState === LOCKED
isDirty === true
```

Lock lost must force:

```text
mode = READ
lockOperationState = IDLE
autosaveEnabled = false
```

Create mode must enforce:

```text
mode = CREATE
lockOperationState = IDLE
autosaveEnabled = false
```

## 6 BUSINESS CONCURRENCY RULES

Multiple users can view same record.
Only one user may edit.
If locked by another session show takeover confirmation.
If takeover accepted, previous session becomes READ and autosave stops.

## 7 LOCK MONITORING

Manager: `manager/LockStatusMonitor.js`

Polling interval comes from runtime configuration.
If lock lost call `LockLostUseCase`.

## 8 HEARTBEAT

Manager: `manager/HeartbeatManager.js`

If backend returns lock lost trigger `LockLostUseCase`.

## 9 CACHE STRATEGY

Manager: `manager/SmartCacheManager.js`

Cache validated via `LastChangeSet.AggChangedOn`.

```text
if abs(dbTimestamp - cacheTimestamp) <= 5500 ms
    use cache
else
    reload
```

Run cache validation on detail open and entering edit.

## 10 RUNTIME CONFIGURATION ENTITY

Backend entity: `RuntimeSettingsSet(Key='GLOBAL')`
Frontend manager: `manager/SettingsManager.js`

## 11 CRITICAL RULE — ALL TIME VARIABLES MUST COME FROM BACKEND

All time values must come from backend runtime config and be read from `stateModel /timers`.
No hardcoded timer intervals.

## 12 TIMER SANITIZATION

Files:

```text
util/runtime/TimerDefaults.js
util/runtime/TimerSanitizer.js
util/RuntimeTimerSanitizer.js
```

Invalid values fallback to defaults and/or clamped ranges.

## 13 MANAGERS USING TIMERS

Must read from `/timers`:

```text
HeartbeatManager
LockStatusMonitor
AutoSaveCoordinator
ActivityMonitor
GCDManager
ConnectivityCoordinator
SmartCacheManager
BeaconManager
```

## 14 WORKFLOW STATE MACHINE

Derived booleans:

```text
isEditableMode = mode === EDIT || mode === CREATE
canEnterEdit = mode === READ && activeObjectId !== "__CREATE"
canAutoSave = mode === EDIT && lockOperationState === LOCKED && isDirty
```

## 15 STATE TRANSITIONS

- Open Detail → READ + IDLE + autosave false
- Create Draft → CREATE + `__CREATE` + IDLE + autosave false
- Enter Edit success → EDIT + LOCKED + autosave true
- Exit Edit → READ + IDLE + autosave false
- Autosave only in EDIT+LOCKED+DIRTY
- Save: CREATE via `CreateChecklist`, EDIT via `SaveChanges`
- Lock Lost: stop heartbeat/autosave and force READ+IDLE
- Close Detail: splitLayout single, clear selectedId, navigate search

## 16 UI BUGS

- In `css/style.css`, remove override `transform: none !important` for `.sapMSwtHandle`.
- In `DetailControlRail.fragment.xml`, second row starts with Close, Edit Switch, Save.
- In `Search.view.xml`, Export must move to overflow; never `priority="Disappear"`.
- Fix overflow/z-index overlap issues.

## 17 BACKEND ALIGNMENT

Backend in `mock_gate_way/`.
Verify entities:
`RuntimeSettingsSet`, `DictionaryItemSet`, `PersonVHSet`, `GetHierarchy`, `LastChangeSet`, `CreateChecklist`, `SaveChanges`, `AutoSave`.

## 18 BACKEND SEED DATA

Ensure seed data for GLOBAL runtime settings, dictionaries, persons, and location hierarchy.

## 19 BACKEND RUNTIME SETTINGS PAYLOAD

Runtime payload must include timer fields used by frontend.

## 20 DIAGNOSTIC LOGGING

Temporary logs in:
`EnterEditUseCase`, `SaveDetailUseCase`, `AutosaveDetailUseCase`, `LockLostUseCase`, `SettingsManager`, `HeartbeatManager`, `LockStatusMonitor`.
Remove after validation.

## 21 BACKEND CURL VALIDATION

Validate:
- `GET /RuntimeSettingsSet(Key='GLOBAL')`
- `GET /DictionaryItemSet?$filter=Domain eq 'LPC'`
- `GET /PersonVHSet?$top=10`
- `GET /GetHierarchy`

## 22 SUCCESS CRITERIA

Search, detail open, create/edit, autosave/save, lock takeover, dictionaries, suggestions,
value help, backend timers, responsive layout, switch visibility, export overflow, cache validation.

## 23 REQUIRED OUTPUT

Provide:
1) root cause analysis
2) minimal safe fixes
3) diff patches
4) backend patches
5) seed additions
6) manual checklist

## 24 ARCHITECTURE GUARD RULES (CRITICAL)

1. Controller isolation (no adapters/backend access).
2. Usecase responsibility (business logic only; no DOM/UI controls).
3. Adapter responsibility (backend-only integration + normalization).
4. State ownership in state model canonical paths.
5. Lock truth source only `state>/lockOperationState`.
6. Selected model usage for UI field binding.
7. Timer config from runtime settings only.
8. Delta payload builder isolation in `util/DeltaPayloadBuilder.js`.
9. Cache logic isolated in `manager/SmartCacheManager.js`.
10. Navigation through `CloseDetailUseCase` and `RouteModeCoordinator`.

## 25 CODING STYLE REQUIREMENTS

- Usecases: pure logic flow (read state → validate → call ports → update state).
- Adapters: build query → call backend → normalize.
- Managers: start/stop timers, react to state; no business data mutation.

## 26 UI BINDING RULES

Editable bindings must support EDIT and CREATE.
Switch may remain hidden in CREATE where locking is forbidden.

## 27 BACKEND DATA CONTRACTS

- SaveChanges payload: `{ rootId, sessionId, delta }`
- AutoSave payload: `{ rootId, sessionId, partialDelta }`
- LastChangeSet fields: `RootId`, `AggChangedOn`
- RuntimeSettingsSet includes runtime timer fields.

## 28 REFACTORING POLICY

Refactor only for duplication, unavoidable bugfix, or architecture violation.
Refactoring must be minimal, localized, non-breaking.

## 29 ERROR RECOVERY

Handle lock lost, autosave failure, network disconnect, backend timeout.
Lock lost forces READ; autosave failure keeps dirty state; reconnect resumes autosave.

## 30 DIAGNOSTIC MODE (TEMPORARY)

Add logs for mode transitions, lock changes, timer values, autosave triggers; remove when stable.

## 31 FULL WORKFLOW STATE MACHINE

Primary states:

```text
READ_IDLE
EDIT_LOCKED
CREATE_DRAFT
READ_LOCK_LOST
SAVING
AUTOSAVING
```

State derived from mode, lockOperationState, autosaveEnabled, isDirty, activeObjectId.

## 32 EVENT → USECASE MAPPING

- open detail → OpenDetailUseCase
- create → CreateDraftUseCase
- toggle edit ON/OFF → EnterEdit/ExitEditUseCase
- autosave tick → AutosaveDetailUseCase
- save click → SaveDetailUseCase
- lock stolen → LockLostUseCase
- close detail → CloseDetailUseCase
- load runtime config → ApplyRuntimeSettingsUseCase

Controllers dispatch events only.

## 33 TIMER LIFECYCLE

On app start: load runtime config, apply timers, start monitors.
On EDIT: start heartbeat, enable autosave.
On exit EDIT/lock lost: stop heartbeat and autosave.

## 34 UI RESPONSIVE LAYOUT CONTRACT

- `DetailControlRail.fragment.xml` row 1: status/secondary actions
- Row 2: Close, Edit Switch, Save, others
- Critical actions never disappear on resize
- Use `OverflowToolbarLayoutData`; avoid `priority="Disappear"` for critical controls

## 35 BACKEND SCHEMA EXPECTATIONS

Tables expected:
`runtime_settings`, `dictionary_items`, `persons`, `locations`, `checklists`, `last_change_set`, `locks`.

Runtime settings include timer columns.
Dictionary includes domains: LPC, PROFESSION, TIME_ZONE.

## 36 SYSTEM TEST SCENARIOS

1. Edit lifecycle
2. Lock takeover
3. Create flow
4. Cache validation
5. Responsive layout
6. Runtime timer update propagation

## 37 DEPENDENCY MAP (PROJECT STRUCTURE)

Expected direction:

```text
controllers → facades → usecases → ports → adapters → backend clients
```

Forbidden dependencies:
controller→adapter/backend, usecase→UI control, adapter→UI model/controller.

## 38 MODULE RESPONSIBILITY MATRIX

- Controller: UI events
- Facade: orchestrate usecases
- Usecase: business logic
- Port: interface contract
- Adapter: backend communication
- Manager: runtime orchestration
- CacheManager: caching
- TimerSanitizer: runtime config validation

## 39 FILE-LEVEL AUDIT CHECKLIST

Audit usecases, runtime config files, managers, and UI layout files for transitions,
lock handling, cache validation, timer source, lifecycle, and responsive behavior.

## 40 BACKEND ENDPOINT CONTRACT VERIFICATION

Verify endpoint JSON structure for runtime settings, dictionaries, persons, hierarchy,
last change, create/save/autosave.

## 41 DATA SEEDING RULES

Seed runtime settings GLOBAL row, required dictionary domains, >=20 persons, and location hierarchy.

## 42 END-TO-END SYSTEM VALIDATION

Validate startup config load, search/detail/edit/autosave/save, lock takeover,
create flow, dictionaries, suggestions, location help, and responsive layout.

## 43 PROJECT NAVIGATION MAP

Map folders to architecture layers before changes.

## 44 CRITICAL FILES INDEX

Treat core usecases, runtime config files, runtime managers, adapters, and key UI files as critical.

## 45 CHANGE SAFETY PROTOCOL

Before edits: identify module responsibility, verify dependency rules, and side effects
(state/timers/autosave/locking/navigation).

## 46 REGRESSION PROTECTION RULES

Protect locking, autosave condition, cache tolerance logic, runtime timer loading/sanitization,
and UI visibility/overflow behavior.

## 47 RECOVERY EXECUTION PLAN

1. Runtime config
2. Workflow stabilization
3. Lock monitoring
4. Cache behavior
5. UI layout fixes
6. Backend alignment
7. Full system validation

Final priority: workflow correctness, architecture integrity, minimal changes,
runtime configuration consistency.

## 48 CENTRALIZED TIMECONFIGSERVICE (MANDATORY)

All runtime timer values must be resolved through a centralized service.

Service contract:

```text
TimeConfigService
```

Responsibilities:

- read raw runtime settings from backend payload
- apply sanitization via timer sanitizer utilities
- expose normalized timer map to state model at `/timers`
- provide read API for managers (no direct hardcoded fallback in managers)

Managers must consume only normalized values from state:

```text
state>/timers/*
```

Direct usage of raw backend timer fields in manager code is not allowed.

## 49 HARD-CODED TIMER BAN (STRICT)

Hardcoded timer delays are forbidden in production flow.

Forbidden examples:

```text
setTimeout(fn, 1000)
setInterval(fn, 60000)
```

Allowed pattern:

```text
const ms = stateModel.getProperty('/timers/<name>Ms');
setTimeout(fn, ms);
setInterval(fn, ms);
```

Exception policy:

- test-only code may use fixed delays
- production modules must use runtime config timers

## 50 ENV / CONFIG LOADING ORDER

Configuration initialization must follow deterministic order:

1. load static app defaults
2. load environment overrides (if present)
3. fetch backend runtime config (`RuntimeSettingsSet(Key='GLOBAL')`)
4. sanitize and clamp timer values
5. write normalized config to state model (`/config`, `/timers`)
6. start runtime managers dependent on timers

If backend config fetch fails:

- app continues with sanitized defaults
- error is logged with diagnostics context
- manager startup uses fallback timer set from sanitizers

## 51 OBSERVABILITY REQUIREMENTS

Observability must be available for recovery and postmortem diagnostics.

Minimum telemetry events:

- `runtime.config.loaded`
- `runtime.config.fallback_used`
- `workflow.mode.changed`
- `lock.state.changed`
- `autosave.triggered`
- `autosave.failed`
- `lock.lost.detected`
- `cache.validation.result`

Each event should include:

```text
sessionId
activeObjectId
mode
lockOperationState
timestamp
```

## 52 STATE GUARD RULES

All usecases must enforce explicit state guards before side effects.

Required guards:

- EnterEdit allowed only from READ and non-`__CREATE` object
- Autosave allowed only when EDIT + LOCKED + DIRTY
- Save in CREATE requires `activeObjectId === "__CREATE"`
- Save in EDIT requires lock state LOCKED
- LockLost transition must be idempotent (safe if called repeatedly)

Guard failures must:

- avoid backend mutation
- return structured error/result
- emit diagnostic event

## 53 DELTA PAYLOAD CONTRACT ENFORCEMENT

Delta payload must be built only through `util/DeltaPayloadBuilder.js`.

Contract rules:

- CREATE path builds full create payload for `CreateChecklist`
- EDIT save path builds `delta` for `SaveChanges`
- AUTOSAVE path builds `partialDelta` for `AutoSave`
- payload must include `rootId` and `sessionId` where required by endpoint contract

No usecase may manually compose endpoint payload shape inline when builder exists.
