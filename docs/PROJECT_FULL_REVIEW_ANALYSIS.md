# Full Project Review / Analysis (UI5 + FastAPI mock Gateway)

## 1) Executive summary
This iteration focuses on **duplication hotspots** and **moving logic to reusable methods/utilities** without changing public contracts.

Current architecture is workable, but several files still carry “God-object” responsibilities (especially `Component.js` and `gateway_canonical_api.py`).

## 2) Duplication map (by area)

### A. UI5 Frontend

#### A1) `controller/App.controller.js`
- Before: split/resizer lifecycle logic was spread across multiple methods and duplicated attach/detach entry points.
- Now: lifecycle is centralized via:
  - `_ensureFclResizerAttached()`
  - `_detachFclResizer()`
  - `_destroyFclResizer()`
- Compatibility wrappers (`_bindSplitDrag`, `_unbindSplitDrag`) remain, but only delegate to centralized helpers.

#### A2) `Component.js`
- Router references previously mixed direct `this.getRouter()` calls with local variable usage.
- Tidied to use one router reference (`oRouter`) inside init flow for route hooks/initialize calls.

#### A3) Existing utility split
- Positive state: FCL behavior is already extracted to
  - `util/FlexibleRouter.js`
  - `util/FclResizer.js`
- Remaining recommendation: keep controller as orchestration only and avoid direct DOM/style decisions there.

### B. Backend (FastAPI / canonical OData)

#### B1) `mock_gate_way/api/gateway_canonical_api.py`
- Previously contained repeated write precondition checks and repeated child mutation logic.
- Refactor already introduced reusable helpers for:
  - root loading / errors
  - write preconditions
  - basic/check/barrier updates
  - collection replacement
  - alias JSON body parsing
- Remaining recommendation: split endpoint handlers vs business operations into dedicated service module.

## 3) Concrete refactor recommendations (next steps)

### Priority P1 (safe + high impact)
1. **Extract Component runtime orchestration**
   - New util classes:
     - `util/runtime/RouterGuardCoordinator.js`
     - `util/runtime/ManagerBootCoordinator.js`
     - `util/runtime/FrontendConfigApplier.js`
2. **Extract gateway write operations service**
   - New backend module:
     - `mock_gate_way/services/checklist_write_service.py`
   - Keep API layer thin (request mapping + response envelope only).

### Priority P2
3. Add regression tests for:
   - alias payload normalization
   - `AutoSave`/`SaveChanges` optimistic checks
   - FCL resizer attach/detach idempotency

### Priority P3
4. Introduce frontend cache service facade:
   - `util/ChecklistCacheService.js`
   - unify cache read/write/snapshot diff operations from controllers.

## 4) Contract stability notes
- Canonical OData root and aliases must stay stable.
- Existing UI flow contracts should not change (search/detail/edit/autosave/save/lock).
- Router best-effort query-sync (`?layout=`) should remain backward-compatible with old UI5 hash behavior.

## 5) What was improved in this iteration
- Reduced controller-level duplication around FCL resizer lifecycle.
- Preserved compatibility methods while moving logic into dedicated helper methods.
- Deduplicated backend lock status payload composition (shared helper for collection/entity reads).
- Deduplicated alias payload read/merge helpers (`_read_merged_payload`, `_extract_legacy_body`) to reduce endpoint shim boilerplate.
- Kept behavior stable and non-breaking.
