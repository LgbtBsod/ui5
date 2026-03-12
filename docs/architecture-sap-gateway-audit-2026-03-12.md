# SAP Lead Architect Audit — UI5 → SAP Gateway hardening (2026-03-12)

## 1) Executive outcome

Проект переведен в более строгий Gateway-aligned baseline с практическими исправлениями в runtime-коде (не только документирование):

- устранены P0 contract drift в `sap-gateway-only-gate`;
- устранены нарушения model-path contract;
- восстановлена консистентность i18n контрактов;
- введен единый domain-level контракт путей состояния для модульного переиспользования.

## 2) What was audited

- архитектурные guardrails (layering/gates),
- SAP Gateway runtime contract,
- state-path governance для доменных модулей,
- консистентность `i18n` bundle,
- качество bootstrap/runtime settings orchestration.

## 3) Critical findings and implemented fixes

### P0-1. Gateway contract drift in runtime bootstrap/navigation

**Findings:**
- `sap-gateway-only-gate` ранее падал:
  - non-canonical path detection around lock path usage,
  - missing strict bootstrap signature for `SettingsManager.load(GatewayBackendService)`.

**Implemented fixes:**
1. Убрана raw path зависимость в навигационном runtime — переход на `StatePaths` constants для lock/edit workflow путей.
2. Приведен bootstrap runtime settings flow к canonical gateway loading signature (`SettingsManager.load(GatewayBackendService)` + reload branch).
3. Исправлены синтаксические дефекты в `ComponentInitRuntime.js`, мешающие стабильному runtime execution.

### P0-2. Domain path contract incompleteness

**Findings:**
- отсутствовало подтверждение использования `/ui/busy/global` и полного contract path набора в domain-слое.

**Implemented fixes:**
1. Добавлен единый модуль `service/domain/shared/ModelPathContracts.js`.
2. Подключен в `BootstrapAppUseCase` для использования контрактного пути `UI_BUSY_GLOBAL`.
3. Набор контрактов теперь централизован и пригоден для переиспользования в usecases.

### P0-3. Localization contract mismatch

**Findings:**
- `domain-model-verify` фиксировал расхождение ключей между `i18n.properties` и `i18n_ru.properties`.

**Implemented fixes:**
1. Синхронизированы ключи в обе стороны:
   - добавлены missing RU-ключи из base bundle,
   - добавлены отсутствовавшие в base ключи test-user сценария.
2. Восстановлена keyset parity для стабильного контрактного контроля.

### P1. Runtime module correctness (hardening)

**Findings:**
- `SettingsManager.js` содержал broken define dependency list (синтаксическая ошибка).

**Implemented fixes:**
- dependency list нормализован до корректного namespace и валидного AMD-модуля.

## 4) Modular structure and single-responsibility alignment

В рамках итерации выполнено выравнивание по принципам SRP и reuse:

- **State-path smart contract** вынесен в отдельный shared domain модуль.
- **Navigation runtime** больше не использует локальные string literals для workflow lock/edit state.
- **Bootstrap usecase** использует domain contract constants, что уменьшает patchy-path updates и расхождение в модулях.

## 5) Gateway hardening status after changes

После исправлений ключевые целевые проверки проходят:

- `sap-gateway-only-gate` — PASS
- `domain-model-verify` — PASS
- `model-path-contract-gate` — PASS
- `architecture-gate` — PASS
- `smart-odata-contract-gate` — PASS

## 6) Next recommended wave (remaining best-practice track)

1. Продолжить конвергенцию всех доменных usecase модулей на `ModelPathContracts` (полный отказ от literal paths).
2. Ввести CI gate на запрет новых state-path string literals вне contract modules.
3. Расширить namespace governance для CSS до единого префикса по функциональным зонам (shell/search/detail/analytics) и auto-check в gate.
4. Выполнить безопасное укрупнение shared ui-behavior contracts (busy/error/lock banners) в единый framework policy module.

## 7) Final assessment

Текущая итерация закрыла реальные P0 блокеры SAP Gateway hardening track и улучшила модульность/контрактность кода.
Проект стал ближе к production-grade best practice baseline для long-running UI5 → SAP Gateway evolution.


## 8) Wave-2 hardening (follow-up implementation)

Выполнена следующая волна конвергенции по замечаниям:

1. **Domain usecases convergence to contracts**
   - добавлен `service/domain/shared/DomainStatePaths.js` для state-path полей доменного уровня (`activeObjectId`, `selectedId`, `layout`, `lockOperationPending`, и др.);
   - usecases и shared-runtime модули переведены с literal state-paths на contract constants;
   - в `service/domain` устранены `get("state", "/...")` и `modelPatch("state", "/...")` literal-вызовы.

2. **CI gate: ban new state-path literals in domain layer**
   - добавлен `scripts/ci/no-domain-statepath-literals-gate.js`;
   - gate блокирует появление новых literal state paths в domain usecases (кроме contract-модулей).

3. **CSS namespace governance + auto-check gate**
   - добавлен `scripts/ci/css-namespace-governance-gate.js`;
   - добавлен baseline allowlist `scripts/ci/css-namespace-allowlist.json` для безопасной инкрементальной нормализации;
   - gate предотвращает появление новых class-namespace drift по функциональным зонам (shell/search/detail/analytics).

4. **Shared UI behavior policy consolidation**
   - добавлен `service/framework/UiBehaviorPolicy.js` как единый policy-модуль для busy/banner behavior;
   - `ComponentInitRuntime` переведен на policy-driven banner операции (`setGlobalBanner` / `clearGlobalBanner`) через общий контракт поведения.

## 9) Validation (wave-2)

- `no-domain-statepath-literals-gate` — PASS
- `css-namespace-governance-gate` — PASS
- `sap-gateway-only-gate` — PASS
- `domain-model-verify` — PASS
- `model-path-contract-gate` — PASS
- `architecture-gate` — PASS
- `smart-odata-contract-gate` — PASS


## 10) Wave-3 hardening (extensibility + constants governance)

1. **Behavior extensibility is preserved and strengthened**
   - добавлены `UiBehaviorDefaultHandlers` и `UiBehaviorOverrideHandlers` на базе `BehaviorRegistry`;
   - `UiBehaviorPolicy` переведен на `BehaviorResolver`, поэтому стандартизованное поведение можно расширять override-модулями без форка базовой логики.

2. **Reusable interface constants expanded**
   - добавлен `UiBehaviorConstants` (scope/operations/banner id), чтобы убрать магические строки из policy/runtime поведения;
   - `DomainStatePaths` расширен (`SESSION_ID`) и используется в `DetailRuntimePayload`.

3. **Runtime correctness fix**
   - исправлен синтаксический дефект в `ApplyRuntimeSettingsUseCase` (AMD define), восстановлен валидный runtime module load.

## 11) Weak zones for enterprise production-ready rollout (current)

### A. End-to-end behavior stability gaps (P0)
- Локальный validation playbook фиксирует две нестабильные browser-smoke точки:
  1. `analytics.close` (нестабильный возврат в `EDIT + EDIT_LOCKED`),
  2. `attachments.delete` после staged upload.
- Это блокирует прогнозируемое UX-поведение и повышает риск race-condition в route/lock orchestration.

### B. Mock-vs-real Gateway parity limits (P0/P1)
- Бэкенд в репозитории — mock gateway с flex/preload stub endpoints (`/sap/bc/lrep/flex/*`, `Component-preload.js`).
- Для enterprise readiness нужен обязательный прогон against реальный SAP Gateway landscape (не только mock invariants).

### C. Runtime contract hardening debt (P1)
- Несмотря на green gates, часть UI state path и view-path контрактов остается вне единой domain contract карты (есть `view`-path literals в usecases).
- Следующий этап: контрактные интерфейсы не только для `state`, но и для `view`/`selected` операций, где это критично для согласованности.

### D. UX contract consistency debt (P1)
- Требуется закрепить enterprise UX invariants отдельным gate-пакетом:
  - route-return stability для analytics close,
  - deterministic attachment staged-delete flow,
  - lock-transition visual feedback SLA (timeout/retry/banner cadence).

## 12) Backend uplift required to observe real SAP Gateway behavior in tests

Ниже — what must be added/connected на backend стороне, чтобы frontend тестировался в truly-real Gateway mode:

1. **Real Gateway integration lane (mandatory)**
   - CI lane/стенд с реальным `SERVICE_ROOT` и живыми `FunctionImport`/EntitySet контрактами.
   - Separate smoke profile без mock-specific shortcuts/stubs.

2. **Authorization and identity parity**
   - Полный прогон через productive auth flow (SSO/Principal propagation) вместо `X-Mock-User`-mode.
   - Проверка прав на create/change/delete/lock operations в реальном role model.

3. **Concurrency and lock semantics parity**
   - Reproducible multi-session lock tests на реальном backend (acquire/heartbeat/release/takeover).
   - Проверка SLA heartbeat timeout/lock-lost propagation до UI без mock simplification.

4. **Attachment and media behavior parity**
   - Реальные upload limits, MIME policy, antivirus/content-scanning hooks, latency profile.
   - Проверка staged→persisted attachment lifecycle в backend with realistic delays/errors.

5. **OData metadata/version governance**
   - Automated diff контроль `$metadata` mock vs real Gateway.
   - Hard fail при drift по critical entities/function imports used by frontend.

6. **Operational telemetry contract**
   - Вынести correlation/request-id, error taxonomies и backend diagnostics в production-grade observability pipeline.
   - Унифицировать incident triage: UI error banner ↔ backend log trace ↔ gateway operation id.

## 13) Recommended execution order (enterprise)

1. P0: закрыть 2 нестабильных browser-smoke сценария (`analytics.close`, `attachments.delete`).
2. P0: подключить real Gateway integration lane и metadata drift gate.
3. P1: расширить contract constants на `view/selected` critical paths и закрепить gate.
4. P1: formalize UX behavior SLA gates (lock/route/banner).
5. P2: продолжить CSS namespace normalization, постепенно выжигая allowlist legacy.

## 14) Python backend weak zones impacting SAP Gateway-like tests (deep-dive)

### Weak zone W1 — capabilities contract drift
- До исправления capability payload отдавал non-canonical paths (`/actions/SaveChecklist`, `/actions/AutoSaveChecklist`), что расходится с FunctionImport contract (`/SaveChanges`, `/AutoSave`).
- Риск: фронт или тест-утилиты начинают ориентироваться на legacy alias вместо канонического OData контракта.

### Weak zone W2 — identity mode ambiguity
- Backend поддерживает `X-Mock-User` заголовок; без явной декларации identity mode тестовый контур может давать ложное ощущение parity с productive auth.
- Риск: сценарии с role/authorization на реальном Gateway не воспроизводятся один-в-один.

### Weak zone W3 — route-surface governance gaps
- Без отдельного backend governance теста легко не заметить drift по canonical/non-canonical surface.
- Риск: неожиданное появление alias endpoints и расхождение smoke сценариев.

## 15) Execution plan for backend evolution (Python) + started implementation

### Step 1 (closed): normalize capabilities to canonical Gateway contract
1. Привести `capabilities.paths` к каноническим FunctionImport/entity paths.
2. Добавить `serviceRoot` и `identityMode` в capabilities payload.
3. Закрыть регресс тестами.

**Status:** CLOSED in this iteration.

### Step 2 (closed): add backend governance tests for route and capabilities parity
1. Добавить backend-readiness test suite для canonical paths и identity mode consistency.
2. Зафиксировать compatibility behavior по unprefixed analytics route.
3. Проверять canonical prefixed availability через `SERVICE_ROOT`.

**Status:** CLOSED in this iteration.

### Step 3 (in progress): tighten identity simulation controls
1. Оставить mock-user mode управляемым backend config flag (`ALLOW_MOCK_USER_HEADER`).
2. Подготовить separate strict profile (flag off) для integration lane against real auth semantics.
3. Добавить CI lane, который прогоняет backend tests в strict profile.

**Status:** STARTED (flag introduced), strict CI lane pending.

### Step 4 (planned): real Gateway parity lane
1. Добавить metadata diff between mock and real Gateway.
2. Добавить mult-session lock semantics replay against real backend.
3. Добавить attachment lifecycle parity suite (latency/error profile).

**Status:** PLANNED.

## 16) Completed: full legacy CSS namespace allowlist burn-down (P2)

Выполнено полное выжигание legacy allowlist для CSS namespace governance:

1. Переименованы legacy class names в модульно-ориентированные namespace имена:
   - `accentSwitch` -> `appAccentSwitch`
   - `accentSwitchEditMode` -> `detailAccentSwitchEditMode`
   - `customFilterWrapper` -> `searchCustomFilterWrapper`
   - `analyticsChartCard` -> `searchAnalyticsChartCard`
   - `hasComment` -> `detailHasComment`
   - `theme-switching` -> `appThemeSwitching`
2. Обновлены соответствующие CSS/XML/JS usage points.
3. `css-namespace-governance-gate` переведен в allowlist-free режим (allowlist очищен).

## 17) Namespace governance for folders/modules/variables (executed)

Чтобы закрыть запрос на namespace контроль папок/модулей/переменных в модулях:

1. Добавлен `module-namespace-governance-gate` для `app/**/*.js`:
   - разрешены только dependency namespace prefixes:
     - `PRODUCTION_CONTROL_CHECKLIST/`
     - `sap/`
     - локальные `./` и `../`
2. Gate гарантирует, что в sap.ui.define dependency lists не появляется произвольный/дрейфующий namespace.
3. Текущий статус: PASS.

## 18) Plan status update (requested items)

- P0 browser-smoke нестабильности (`analytics.close`, `attachments.delete`) — **в работе**, требуется отдельный runtime behavior fix-cycle.
- P0 real Gateway integration lane + metadata drift gate — **в работе** (backlog шаги уже формализованы, backend подготовлен к strict profile evolution).
- P1 contract constants expansion (`view/selected`) + gate — **частично закрыто**, продолжать по критическим UX путям.
- P1 UX behavior SLA gates (lock/route/banner) — **planned**, не закрыто полностью.
- P2 CSS namespace allowlist burn-down — **CLOSED** (allowlist-free gate).

## 19) P0/P1/P4 execution (cleanup implementation, no REST fallback)

### P0 — unstable browser-smoke behavior hardening

Implemented cleanup changes targeting the two unstable scenarios:

1. **analytics.close stabilization**
   - edit-lock restore retry logic hardened in detail runtime:
     - retry budget increased (`attempts >= 3` threshold),
     - asynchronous retry delay introduced (`220ms`) вместо zero-delay race.
2. **attachments.delete stabilization**
   - удаление больше не зависит только от `attachmentId` literal;
   - добавлен robust identity resolution (`resolveDeletionId`, `removeByAttachment`) и синхронное обновление both `selected`/`view sessionAttachments` collections.

### P1 — contract constants expansion + behavior SLA governance

1. Added `ViewPathContracts` and migrated critical detail-domain paths (`detailSkeletonBusy`, `attachmentsLoaded`, `sessionAttachments`, `validationShown`, `validationMissing`, `accessState`).
2. Added `domain-viewpath-contract-gate` to block regressions on critical `view` path literals.
3. Added `ux-behavior-sla-gate` to pin critical behavior invariants (analytics restore retries, attachment delete synchronization, unified banner policy usage).

### P4 — real Gateway parity lane tooling

Implemented concrete parity toolkit (non-mock lane):

1. `gateway-metadata-drift-gate.py` — critical `$metadata` token diff (mock vs real).
2. `gateway-lock-multisession-replay.py` — replay lock acquire/heartbeat/release semantics against real backend.
3. `gateway-attachment-lifecycle-parity.py` — attachment upload/delete lifecycle parity against real backend.
4. `gateway-real-parity-suite.sh` — orchestration entrypoint for CI/staging lane.

### No REST/fetch bypass policy (explicit)

- Added `no-rest-bypass-gate` over runtime app modules to block `fetch`, `XMLHttpRequest`, `axios`, `jQuery.ajax` usage in app runtime path.
- This enforces OData-only transport discipline and avoids bypass fallback mechanics.

## 20) Additional UX/UI cleanup pass (current iteration)

1. **Removed redundant route-state condition** in detail route matching flow (`DetailChecklistRuntime`) to reduce branch ambiguity during layout-only transitions.
2. **Cleaned animation state overrides** in background module by dropping unnecessary `!important` flags for explicit background runtime classes.
3. **Cleanup implementation for anti-patch resilience (current hardening)**
   - extracted analytics edit-restore retry policy into dedicated behavior extension surface (`DetailRuntimePolicy` + default/override handlers), so retry cadence is no longer buried as magic numbers in controller runtime;
   - moved additional state path literals to `DomainStatePaths` (`CURRENT_ROUTE_NAME`, `ANALYTICS_RETURN_RESTORE_EDIT`, `AUTOSAVE_ENABLED`, `IS_DIRTY`) to continue model behavior standardization and reduce string-drift;
   - kept runtime orchestration in `DetailChecklistRuntime`, while shifting policy decisions to dedicated SRP module-level contracts for future extensions.

4. **Project-wide anti-patch CSS cleanup continuation**
   - removed residual non-patch `!important` usage across runtime CSS modules;
   - normalized `.sap*` selector scoping under app root to reduce global bleed and make layering explicit;
   - added explicit placeholder patch modules (`90/91/92`) to keep future vendor/UI5 overrides isolated.

5. Re-validated gates for:
   - UX behavior SLA,
   - no REST bypass,
   - domain state/view contract guards,
   - CSS/module namespace governance,
   - SAP gateway-only architecture contracts,
   - backend gateway enterprise readiness tests,
   - CSS architecture contract.

Result: governance suite remains green for runtime safety and OData-only discipline.
