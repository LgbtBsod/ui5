# SAP UI5 1.71 Architectural Audit Report

## Scope and constraints
- UI5 runtime baseline: 1.71.x.
- Integration target: OData V2 via `/sap/opu/odata/sap/Z_UI5_SRV/`.
- Smart controls target: `sap.ui.comp` SmartFilterBar + SmartTable using backend-side filtering/paging.

## 1) Project analysis summary

### Module inventory (static)
- Controllers: 5
- Views/fragments: 12
- Services (incl. use cases): 95
- Managers: 10
- Utils: 17
- Models: 2
- Controls: 1

Primary dependency hubs (sap_ui5 imports):
- `service/backend/BackendAdapter`
- `util/FlowCoordinator`
- `service/usecase/DetailLifecycleUseCase`
- `service/backend/GatewayClient`

### Routing + entrypoints
- Search is entry route (`pattern: ""`).
- Detail routes are checklist-oriented (`checklist/{id}`, `checklist/{id}/{layout}`).

### OData model shape
- Default OData V2 model configured with:
  - `serviceUrl: /sap/opu/odata/sap/Z_UI5_SRV/`
  - `useBatch: true`
  - `defaultBindingMode: "TwoWay"`
  - `defaultCountMode: "Inline"`
  - `tokenHandling: true`

## 2) Search flow audit

### Current flow
1. `Search.controller` boots Smart controls.
2. SmartFilterBar (`ChecklistSearchSet`) emits search/filter events.
3. SmartTable (`ChecklistSearchSet`) rebinds with merged filters.
4. Rebind parameters add segmented status filters:
   - FAILED → `HasFailedChecks eq true`
   - SUCCESS → `HasFailedChecks eq false`
   - ALL → no extra filter
5. Data remains server-driven via OData query options (`$filter`, `$top`, `$skip`, `$inlinecount`).

### Notes
- SmartTable/SmartFilterBar are correctly bound to OData entity set.
- No legacy `/checklist` REST calls in UI runtime path.

## 3) Detail flow audit

### Current flow
- Route match loads detail context by key.
- Separate entity reads (no forced `$expand`) for root/basic/checks/barriers/attachments.
- Lock + save lifecycle uses Gateway function imports and entity calls through backend adapter/client.

### Notes
- Controller delegates large parts to use case modules.
- Architecture already follows Controller → UseCase → backend client pattern in most business flows.

## 4) Duplicate logic and consolidation status
- Search and Detail already rely on use-case orchestration modules for:
  - busy/lifecycle orchestration,
  - save/error presentation,
  - rebind/filter orchestration,
  - lock/status management.
- Remaining cleanup should be evolutionary (avoid high-risk big-bang rewrites).

## 5) Legacy mock-backend status
- Removed from runtime code path:
  - `service/backend/FakeBackendService.js`
  - `service/backend/FakeODataService.js`
  - `service/backend/InMemoryDB.js`
- `BackendAdapter` is real-backend oriented.

## 6) Safe delete list status
Deleted (confirmed obsolete in runtime path):
- `service/backend/FakeBackendService.js`
- `service/backend/FakeODataService.js`
- `service/backend/InMemoryDB.js`

Retained intentionally:
- Theme switch modules (`control/ThemeToggle.js`, `util/ThemePhilosophy.js`) due explicit product feedback to keep theme switch.

## 7) Manifest/library audit
- Libraries remain compatible with 1.71 and app usage (`sap.ui.core`, `sap.m`, `sap.ui.comp`, `sap.ui.layout`, `sap.f`, plus currently used `sap.ui.table`, `sap.uxap`).
- No `sap.ui.mdc`, `sap.fe`, or Fiori Elements introduced.

## 8) Network behavior expectation check
Expected runtime pattern remains:
- `GET $metadata`
- SmartTable/SmartFilterBar-driven reads for `ChecklistSearchSet` with server-side options
- Batch-enabled OData model for grouped operations
- Detail page loads via separate entity requests (Gateway-like style)

## 9) Action taken in this change set
- Updated unit smoke coverage for `SearchApplicationService` to align with current OData/Gateway client contract (GatewayClient-based read behavior and error propagation).

## 10) Recommendations (next incremental iteration)
1. Continue moving any residual controller condition branches into dedicated use cases.
2. Add dedicated unit tests for segmented filter → OData filter conversion.
3. Add contract tests for lock/autosave function import payload schemas.
4. Keep static dependency audit in CI to prevent reintroduction of local backend emulation.
