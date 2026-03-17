# CODEX EXECUTION PLAYBOOK V1

Use this file as the execution map for the current repository.
It tells you which files to modify, in what order, and what each wave is supposed to achieve.

---

## Wave 1 — runtime contract sync

### Goal
Bring frontend, mock backend, and ABAP backend to the same timing and lock contract.

### Files
- `app/model/StateSchema.js`
- `app/service/framework/TimerDefaults.js`
- `app/service/framework/RuntimeTimerSanitizer.js`
- `app/service/framework/TimeConfigService.js`
- `app/localService/metadata.xml`
- `backend/mock_gateway/config.py`
- `backend/mock_gateway/models.py`
- `backend/mock_gateway/services/settings_service.py`
- `backend/mock_gateway/api/settings_api.py`
- `backend/mock_gateway/services/metadata_builder.py`
- `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap`
- `backend/sap_backend/src/zfg_zodata_lock.fugr.abap`

### Expected result
- production profile exists
- test profile exists
- production TTL = 600 sec
- production timer values match the contract
- mock/backend/frontend use the same names and units
- old 15-second production assumptions are removed from mainline behavior

---

## Wave 2 — edit-session coordinator

### Goal
Centralize edit-session timer ownership.

### Files
- `app/service/runtime/ActivityMonitor.js`
- `app/service/runtime/HeartbeatManager.js`
- `app/service/runtime/LockStatusMonitor.js`
- `app/service/runtime/AutoSaveCoordinator.js`
- `app/service/runtime/GCDManager.js`
- `app/service/framework/ComponentPollingRuntime.js`
- `app/service/framework/ComponentLockEventsRuntime.js`
- `app/service/framework/ComponentRuntimeHandlerRuntime.js`
- `app/service/domain/shared/usecases/StartManagersUseCase.js`

### Expected result
- one edit-session coordinator owns:
  - heartbeat
  - lock status
  - autosave
  - inactivity
  - refresh cooldown
- inactivity timeout fully tears down edit timers
- activity event allowlist uses:
  - `click`
  - `keydown`
  - `input`
  - `change`
  - `scroll`
  - `touchstart`
- `mousemove` is removed

---

## Wave 3 — write-path truth and lock-lost transition

### Goal
Make save/autosave responses authoritative for lock validity.

### Files
- `app/service/framework/ComponentSaveGuardRuntime.js`
- `app/service/framework/ComponentAutosaveRuntime.js`
- `app/service/domain/detail/DetailPersistenceRuntime.js`
- `app/service/domain/detail/usecases/SaveDetailUseCase.js`
- `app/service/domain/detail/usecases/AutosaveDetailUseCase.js`
- `app/service/domain/detail/usecases/ForceReadOnlyUseCase.js`
- `app/service/domain/detail/usecases/LockLostUseCase.js`
- `app/service/backend/GatewayErrorNormalizer.js`
- `backend/mock_gateway/services/lock_service.py`
- `backend/mock_gateway/api/gateway_canonical_api.py`
- `backend/mock_gateway/tests/test_lock_service_contract.py`

### Expected result
- save/autosave validate lock, owner session, TTL, permissions
- lock-related save/autosave failures immediately trigger readonly transition
- no attempt to preserve editable local draft on lock loss in v1
- reset/reload from server snapshot after lock loss
- cooldown starts only if backend confirmed refresh

---

## Wave 4 — persistence status UI and i18n cleanup

### Goal
Make save/autosave state explicit and truthful.

### Files
- `app/views/fragment/DetailControlStatusRow.fragment.xml`
- `app/service/features/detail/runtime/DetailFormatters.js`
- `app/i18n/i18n.properties`
- `app/i18n/i18n_en.properties`
- `app/i18n/i18n_ru.properties`

### Expected result
- manual save shows:
  - `Saving…`
  - `Saved`
  - `Not saved`
- autosave shows:
  - `Autosaving…`
  - `Saved`
  - `Not saved`
- backend error detail available in tooltip or secondary message
- remaining English leftovers in RU bundle are cleaned

---

## Wave 5 — analytics route lifecycle and localization

### Goal
Make full analytics page lifecycle route-scoped and localized.

### Files
- `app/controller/analytics/AnalyticsLifecycleBehavior.js`
- `app/controller/analytics/AnalyticsLoadBehavior.js`
- `app/controller/analytics/AnalyticsControllerBehavior.js`
- `app/controller/analytics/AnalyticsLoadRuntime.js`
- `app/controller/analytics/AnalyticsRefreshRuntime.js`
- `app/views/Analytics.view.xml`
- `app/views/fragment/WorkflowAnalyticsTrends.fragment.xml`
- `app/service/features/analytics/runtime/AnalyticsExportRows.js`
- `app/i18n/i18n.properties`
- `app/i18n/i18n_en.properties`
- `app/i18n/i18n_ru.properties`

### Expected result
- full analytics route loads on enter
- starts 15-minute timer on enter
- clears timer on leave
- reloads and restarts timer on re-enter
- analytics labels and month names use i18n everywhere

---

## Wave 6 — cache and tooling cleanup

### Goal
Align the repo with the chosen simple cache model and current style architecture.

### Files
- `app/service/domain/cache/usecases/CacheValidationUseCase.js`
- `app/service/contracts/FrontendConfigConstants.js`
- `docs/generated-artifacts.md`
- `docs/network-contract-report.md` if touched by current naming
- `scripts/report-css-class-coverage.js`

### Expected result
- cache is documented and named as stamp-based validation only
- no misleading TTL-style freshness contract if not implemented
- CSS coverage script scans `app/styles/**` instead of legacy `app/css/**`

---

## Required output from Codex

When implementation is done, return:
1. changed files grouped by wave
2. short explanation of each changed file
3. tests added or updated
4. explicit remaining backend or environment assumptions
5. any behavior change that was necessary to preserve runtime correctness
