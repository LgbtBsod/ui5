# Project Review & Refactor Plan (UI5 + FastAPI Gateway mock)

## Scope reviewed
- UI5 app lifecycle/router/FCL integration (`Component.js`, `controller/*`, `util/*`, `manifest.json`).
- Backend canonical OData API (`mock_gate_way/api/gateway_canonical_api.py`, related middleware/contracts).
- Cross-cutting: lock flow, autosave/save/status, route/layout state, dictionary/hierarchy/contracts.

## Key duplication/complexity hotspots

### 1) Backend canonical API (`gateway_canonical_api.py`)
**Before refactor** there were repeated blocks for:
- loading root by key + repeated not-found checks,
- repeated If-Match / ClientAgg / optimistic validation,
- duplicated CHECK/BARRIER C/U/D mutation logic,
- duplicated child collection replacement logic for full save,
- repeated JSON body read in alias handlers,
- alias hierarchy call path with drift from canonical FI signature.

**What is now extracted:**
- `_load_root_or_error(...)`
- `_validate_write_preconditions(...)`
- `_apply_basic_fields_update(...)`
- `_create_check(...)` / `_create_barrier(...)`
- `_apply_change_row(...)`
- `_replace_child_collections(...)`
- `_read_json_request(...)`

Result: less repeated business logic and lower risk of contract drift between `AutoSave` / `SaveChanges` / `SetChecklistStatus`.

### 2) FCL + split logic
- Project now has dedicated utilities (`util/FlexibleRouter.js`, `util/FclResizer.js`) and controller wrapper methods for compatibility.
- Further cleanup recommended: remove remaining legacy split-signature state logic from `App.controller.js` after UI regression pass.

### 3) Component bootstrap
- `Component.js` is still a large orchestration file with many concerns mixed in (models, runtime config, lock managers, autosave, route guards, diagnostics).
- Recommended next extraction:
  - `util/runtime/RuntimeConfigApplier.js`
  - `util/runtime/ManagerLifecycleCoordinator.js`
  - `util/runtime/RouteGuardOrchestrator.js`

## Contract-risk checklist
- Keep canonical root `/sap/opu/odata/sap/Z_UI5_SRV/` as source of truth.
- Keep deprecated aliases (`/actions/*`, `/lock/*`, `/ChecklistRoots`, `/SearchRows`) as shims only.
- Any new fields should be added via common mapping helpers (avoid per-endpoint field literals).

## Prioritized next refactor iterations
1. **P1:** split `Component.js` orchestration into 2–3 focused coordinators.
2. **P1:** move OData alias adaptation mapping into dedicated adapter module.
3. **P2:** move attachment in-memory registry to service class with explicit interface + tests.
4. **P2:** introduce minimal backend tests around precondition helpers and alias payload normalization.
5. **P3:** unify frontend cache model read/write through one `CacheService` utility to avoid path-level duplication in controllers.

## Acceptance criteria for next iterations
- No endpoint contract changes.
- Existing UI flows remain: Search -> Detail -> Edit/Save/AutoSave/Lock heartbeat/status.
- No regressions for CSRF, If-Match/optimistic checks, and LastChangeSet cache-check behavior.
