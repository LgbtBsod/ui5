# Аудит текущей структуры и план продолжения рефакторинга (2026-02)

## 1) Срез архитектуры на текущий момент

### UI слой
- `controller/Search.controller.js` — центр orchestration для поиска, SmartTable/SmartFilter fallback, KPI и навигации.
- `controller/Detail.controller.js` — сценарии жизненного цикла карточки (read/edit/save/lock/autosave + derived-state).
- `view/*` + `view/fragment/*` — композиция экранов и диалогов, включая rail и analytics dialog.

### Application / use-case слой
- `service/usecase/SearchApplicationService.js` — объединяет search-flow для backend + fallback.
- `service/usecase/WorkflowAnalyticsUseCase.js` — analytics для workflow/KPI.
- `service/usecase/ChecklistCrudUseCase.js` — CRUD orchestration.

### Domain / util слой
- `util/SearchWorkflowOrchestrator.js` — нормализация строк и расчет KPI.
- `util/ChecklistValidationService.js`, `util/DeltaPayloadBuilder.js`, `util/FlowCoordinator.js` — валидация/дельта/flow policy.

### Infra слой
- `service/backend/*` — адаптер и реализации fake/real backend.
- `Component.js` — wiring моделей, metadata-health, lock/heartbeat/autosave/monitoring.

## 2) Выявленные проблемные зоны

1. **Перегрузка контроллеров orchestration-логикой**
   - Search/Detail контроллеры остаются крупными и совмещают UI-реакции, форматирование, state-reduction и инфраструктурные ветки.
2. **Дублирование метрик и summary-обновлений в Search**
   - KPI и `resultSummary` считались в нескольких местах разными путями.
3. **Слабая сегментация состояния ViewModel**
   - В контроллерах много точечных `setProperty` без единого apply-слоя для группы полей.
4. **Ограниченное автоматизированное покрытие refactor-модулей**
   - use-case/util модули готовы к unit-тестам, но покрытие пока не введено.

## 3) Что продолжено в этом шаге рефакторинга

### 3.1 Search metrics-path унифицирован
- В `util/SearchWorkflowOrchestrator.js` добавлены:
  - `resolveTotal(iTotalOverride, iVisible)`;
  - `buildMetrics(aRows, iTotalOverride)` как единая точка для KPI+summary input.
- В `controller/Search.controller.js`:
  - добавлен `_getViewModel()` для единообразного доступа к view-model;
  - добавлен `_applySearchMetrics()` и `_buildResultSummaryText()`;
  - `_updateSmartTableAnalytics()` и `_updateResultSummary()` переведены на единый metrics apply-path.

### 3.2 Metadata recovery path закреплен
- Логика восстановления smart-controls при восстановлении metadata сохранена: при переходе из failed->ok smart-controls снова включаются и bootstrap запускается повторно.

### 3.3 Thin formatter extraction (Detail)
- Создан `util/DetailFormatters.js` и перенесены функции форматирования статусов/валидации/итогов.
- `controller/Detail.controller.js` теперь делегирует форматирование в util-модуль, сохраняя сигнатуры formatter-методов для XML bindings.

### 3.4 Smart-control coordinator extraction (Search)
- Добавлен `util/SearchSmartControlCoordinator.js` с логикой availability sync, bootstrap, wiring inner table и extraction checklist-id.
- `controller/Search.controller.js` делегирует smart-control lifecycle в coordinator, что уменьшает инфраструктурный код внутри UI-контроллера.

## 4) Итог Wave 8 и переход к Wave 9

### Wave 8 — завершено
1. Search metrics-path унифицирован (`SearchWorkflowOrchestrator` + единый `_applySearchMetrics`).
2. Metadata recovery path закреплен (auto re-enable smart-controls).
3. Thin formatter extraction для Detail выполнен (`util/DetailFormatters.js`).
4. Smart-control coordinator extraction выполнен (`util/SearchSmartControlCoordinator.js`) включая availability sync, bootstrap, selection extraction, rebind/fallback и rebind filter policy.


### Wave 9 — завершено
1. Базовый lifecycle usecase для Detail внедрен (`DetailLifecycleUseCase`) и применен в controller state transitions.
2. Введен smoke-unit baseline (`scripts/unit-smoke.js`).
3. Добавлены smoke-unit проверки для ключевых util/usecase модулей:
   - `DetailLifecycleUseCase`
   - `SearchSmartControlCoordinator`
   - `SearchWorkflowOrchestrator`
   - `ChecklistValidationService`
   - `DeltaPayloadBuilder`

### Wave 10 — завершено
1. Стартовал вынос command-flow логики из контроллеров:
   - `DetailCommandFlowUseCase` для решений close/toggle-edit/unsaved decision.
   - `SearchActionUseCase` для action-level mapping (analytics payload, export rows, selected id, unsaved decision policy).
2. Контроллеры (`Detail.controller`, `Search.controller`) переведены на делегирование этих decision/action участков в usecase-модули.
3. Smoke-unit baseline расширен проверками новых Wave 10 модулей (`SearchActionUseCase`, `DetailCommandFlowUseCase`).

### Wave 11 — завершено
1. Вынесены дополнительные command/orchestration policy участки из контроллеров:
   - `DetailSaveConflictUseCase` (reload/overwrite conflict choice routing).
   - `SearchUiFlowUseCase` (selection id extraction, export menu entity resolve, dialog presence helper).
2. `Detail.controller` и `Search.controller` делегируют соответствующие decision/orchestration branches в новые usecase-модули.
3. Smoke harness расширен до CI-friendly режима `--json` и добавлен hook-скрипт `scripts/pre-push-smoke.sh`.

### Wave 12 — завершено
1. Вынесены дополнительные orchestration/usecase ветки:
   - `DetailEditOrchestrationUseCase` для toggle-edit orchestration (confirm/disable/release/acquire/recover/error flow).
   - `SearchUiFlowUseCase` интегрирован в `Search.controller` для selection/export/dialog flow decisions.
   - `DetailSaveConflictUseCase` интегрирован в save conflict choice routing.
2. Обновлен smoke harness до async runner модели с поддержкой `--json` отчета о прохождении test-cases.
3. Добавлен pre-push smoke hook `scripts/pre-push-smoke.sh` как fail-fast quality gate для локального CI-like потока.

### Wave 13 — завершено
1. Продолжена декомпозиция controller orchestration:
   - `DetailStatusRowUseCase` вынес status/row/dialog policy (status-change guards, info-card value mapping, delete-sync guard, expanded-dialog meta).
   - `SearchAnalyticsExportUseCase` вынес analytics dialog open/close и export promise routing.
2. `Detail.controller` и `Search.controller` переключены на делегирование соответствующих веток в новые usecase-модули.
3. Smoke-unit coverage расширена для Wave 13 модулей (`DetailStatusRowUseCase`, `SearchAnalyticsExportUseCase`).

### Wave 14 — завершено
1. Вынесены дополнительные command handlers и orchestration usecases:
   - `DetailStatusCommandUseCase` (status-change command flow + delete-row sync handling).
   - `SearchAnalyticsDialogExportFlowUseCase` (analytics dialog orchestration + export run flow).
2. `Detail.controller` и `Search.controller` обновлены на делегирование этих веток.
3. Smoke coverage расширена тестами для новых Wave 14 модулей.
4. Подготовлен migration guide `docs/usecase-coordinator-migration-guide.md` и CI quality-gate script `scripts/ci-smoke-gate.js` для smoke-json отчета.

### Wave 15 — завершено
1. Продолжен вынос save/row/dialog command веток из `Detail.controller`:
   - `DetailRowDialogCommandUseCase` (row path/dialog id/delete-result policy).
   - `DetailStatusCommandUseCase` используется как единая status-change command orchestration точка.
2. Продолжен вынос intent/adapters из `Search.controller`:
   - `SearchIntentUseCase` интегрирован для search intent и status-filter apply flow.
   - export/analytics orchestration продолжает делегироваться в dedicated usecases.
3. Smoke suite расширен дополнительными сценариями:
   - backend analytics fallback (`WorkflowAnalyticsUseCase` fallback path).
   - lock conflict recovery branch (`DetailEditOrchestrationUseCase`).
4. Подключен CI workflow `.github/workflows/smoke-gate.yml` с обязательным шагом `scripts/ci-smoke-gate.js` поверх smoke-json отчета.

### Wave 16 — завершено
1. Вынесены дополнительные save/detail orchestration ветки:
   - `DetailSaveOrchestrationUseCase` (save + reload collection + apply result + error hook flow).
   - `SearchLoadFilterUseCase` (retry-load/reset-filters/search-mode intent flow).
2. Контроллеры `Detail.controller` и `Search.controller` переведены на делегирование соответствующих веток.
3. Smoke suite расширен интеграционными сценариями metadata recovery + smart-control fallback и новыми usecase тестами.
4. CI smoke-gate workflow продолжает работать как обязательный quality gate.

### Wave 17 — завершено
1. Дальнейшая декомпозиция dialog lifecycle участков в `Detail.controller` выполнена через `DetailDialogLifecycleUseCase` (lazy open + destroy/reset вынесены из контроллера).
2. `Search.controller` дополнительно сокращен через extraction presentation/formatter и maxRows-normalization в `SearchPresentationUseCase`.
3. Smoke baseline расширен тестами `SmartSearchAdapter.filterData` (EXACT/LOOSE + failed-status fallback по `successRate*`).
4. Регулярный отчет архитектурных метрик (`scripts/report-architecture-metrics.js`) продолжает использоваться как wave-audit checkpoint.

### Wave 18 — завершено
1. Закрыта задача legacy CSS: содержимое `css/a_style.css` перенесено в `css/style.css`, отдельный файл удален, подключение стилей унифицировано.
2. Продолжен вынос dialog lifecycle orchestration в usecase-слой (`DetailDialogLifecycleUseCase`) и presentation-веток в `SearchPresentationUseCase`.
3. Smoke baseline расширен regression-сценариями для `SmartSearchAdapter` и presentation/dialog usecases.
4. Архитектурный контроль wave-метрик подтвержден регулярным запуском `scripts/report-architecture-metrics.js`.

### Wave 19 — завершено
1. Продолжен вынос save/edit orchestration из `Detail.controller`: lock/edit toggle flow вынесен в `DetailLockEditFlowUseCase`.
2. Добавлен browser-level smoke сценарий metadata recovery + smart-controls fallback rendering (`scripts/browser-smoke-metadata-recovery.py`).
3. Подготовлен semantically named CSS decomposition plan для `css/style.css` с сохранением единой точки подключения (`docs/style-css-decomposition-plan-wave19.md`).
4. Smoke-gate усилен дополнительными негативными smoke-сценариями для workflow analytics/export/error handling в `scripts/unit-smoke.js`.

### Wave 20 — завершено
1. Продолжен extraction lock/save conflict branches из `Detail.controller`:
   - `DetailLockReleaseUseCase` для release-lock flow c гарантированным UI-idle finalize.
   - `DetailSaveConflictFlowUseCase` для routing conflict-choice handler в save error branch.
2. Browser-smoke metadata-recovery интегрирован в CI как nightly/non-blocking lane (`browser-smoke-metadata-recovery` job в `.github/workflows/smoke-gate.yml`).
3. Выполнена section-based реорганизация `css/style.css` по плану Wave 19 через семантические разделы и явные section headers без смены runtime entrypoint.
4. Smoke-гейт расширен негативными сценариями для lock-release/save-conflict/retry error branches в `scripts/unit-smoke.js`.

### Wave 21 — завершено
1. Продолжен вынос save orchestration branches из `Detail.controller`:
   - `DetailSaveSuccessFlowUseCase` для post-save UX hooks (model refresh + lifecycle + notifications).
   - save-conflict handler wiring оставлен в `DetailSaveConflictFlowUseCase` как adapter-level routing point.
2. Добавлен browser-smoke сценарий для export/analytics dialog flow (`scripts/browser-smoke-analytics-export-flow.py`) и подключен в nightly/non-blocking CI lane.
3. Нормализован legacy migration block в `css/style.css`: удален дублирующий tail-block, токены `--radius-*` и `--transition` оставлены в едином source-of-truth блоке.
4. Расширен smoke/ci-gate сценариями для save-conflict reload/overwrite веток и post-save success flow на уровне адаптеров.

### Wave 22 — завершено
1. Продолжено снижение orchestration-доли в `Detail.controller`:
   - close-flow вынесен в `DetailCloseFlowUseCase`.
   - post-save success orchestration закреплен в `DetailSaveSuccessFlowUseCase`.
2. Добавлен browser-smoke сценарий для detail lock lifecycle (`scripts/browser-smoke-detail-lock-lifecycle.py`) и подключен в nightly/non-blocking CI lane.
3. Выполнен cleanup/de-dup pass для CSS utility selectors: удален дублирующий utility-block `.floatingControlSausage .sapMBtn { width: 100%; }`.
4. Расширено smoke negative coverage для backend timeout/retry branches (`SearchApplicationService` fallback и `DetailSaveOrchestrationUseCase` error branch).

### Wave 23 — завершено
1. Продолжен extraction close/save navigation UX веток из `Detail.controller`:
   - close navigation decision/proceed wiring вынесен через `DetailCloseNavigationUseCase` + `DetailCloseFlowUseCase`.
2. Добавлен browser-level smoke сценарий для detail save-conflict path (`scripts/browser-smoke-detail-save-conflict-flow.py`) и включен в nightly/non-blocking CI lane.
3. Проведен CSS clean pass для responsive override selectors: удален повторяющийся override-блок `floatingControlSausage` в media section.
4. Расширен smoke/ci-gate сценариями для backend conflict/timeout комбинаций в search/detail flows (`SearchApplicationService`, `DetailSaveOrchestrationUseCase`, `DetailSaveConflictFlowUseCase`).

### Wave 24 — завершено
1. Продолжено снижение orchestration-доли в `Detail.controller`: toolbar/validation status-change ветки вынесены в `DetailToolbarValidationUseCase`.
2. Browser-smoke сценарий `scripts/browser-smoke-detail-save-conflict-flow.py` усилен runtime decision/message assertions для reload/overwrite/unknown choice веток.
3. Проведен cleanup pass по CSS повторяющимся theme override selectors: legacy-блоки `.themeDockButton*` дедуплицированы в пользу единого late override.
4. Расширен smoke coverage для backend flaky/network error recovery в search/detail pipeline (`SearchApplicationService`, `DetailSaveOrchestrationUseCase`) в `scripts/unit-smoke.js`.

### Wave 25 — завершено
1. Продолжена декомпозиция `Detail.controller`: save error presentation + backend error mapping adapters вынесены в `DetailSaveErrorPresentationUseCase`.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-toolbar-status-flow.py` для toolbar status switch interaction (validate → status apply → save success toast path) и подключен в nightly/non-blocking CI lane.
3. Выполнен CSS cleanup pass по duplicated status-toggle/theme polish selectors: удалены повторяющиеся late override блоки и сохранен единый baseline для status-toggle.
4. Усилено smoke coverage негативными сценариями lock-acquire retry exhaustion + save retry abort в detail pipeline (`DetailEditOrchestrationUseCase`, `DetailSaveErrorPresentationUseCase`).
5. Search UX harmonization follow-up: устранено дублирование action-кнопок между rail/table toolbar, theme-dock переведен на контекстные moon/sun иконки, SmartFilterBar/SmartTable переведены на metadata-first режим (backend OData filters/columns) с fallback-панелью только для degraded режима.

### Wave 26 — завершено
1. Продолжена декомпозиция `Detail.controller`: expanded-dialog row mutation/lifecycle ветки вынесены в `DetailExpandedRowsFlowUseCase` (open/close/add/delete/sync).
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-expanded-rows-lifecycle.py` для detail expanded rows lifecycle (open/add/delete/close) с runtime assertions по selected-model sync и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для message/dialog glass overrides: устранено пересечение message-dialog и unified dialog веток, сохранен единый стиль в section-level override блоке.
4. Расширен smoke coverage для metadata degraded mode + fallback edit restrictions в search/detail связке (`SearchSmartControlCoordinator`, `DetailStatusCommandUseCase`, `DetailExpandedRowsFlowUseCase`) в `scripts/unit-smoke.js`.

### Wave 27 — завершено
1. Продолжена декомпозиция `Search.controller`: выделен `SearchSelectionNavigationUseCase` для selection/navigation + result-panel selection-state synchronization.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-search-selection-lifecycle.py` для search selection lifecycle (select → navigation intent → cancel/back → restore selection state) и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для search rail/toolbar responsive overrides: объединены quick-filters mobile branches и нормализован adaptive stack для `.searchToolbar`.
4. Расширен smoke coverage для selection persistence + navigation cancellation branches в search/detail cross-flow (`SearchSelectionNavigationUseCase`) в `scripts/unit-smoke.js`.

### Wave 28 — завершено
1. Продолжена декомпозиция `Search.controller`: вынесен `SearchSmartFilterFlowUseCase` для SmartFilter state-sync + rebind preparation policy.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-smartfilter-fallback-transition.py` для SmartFilter fallback transitions (metadata ok → degraded → recovery) с UI assertions по toolbar/filter visibility и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для smart table/fallback table density/spacing overrides: объединены table-row visual branches и унифицированы row spacing/rounding селекторы.
4. Расширен smoke coverage для SmartFilter max-results normalization + rebind parameter policy branches (`SearchSmartFilterFlowUseCase`, `SearchSmartControlCoordinator`) в `scripts/unit-smoke.js`.

### Wave 29 — завершено
1. Продолжена декомпозиция `Search.controller`: выделен `SearchWorkflowAnalyticsDialogUseCase` для workflow analytics dialog lifecycle + busy/error presentation policy.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-analytics-dialog-lifecycle.py` для analytics dialog lifecycle (open → load fallback/success → close/reopen) с assertions по busy/error/summary state и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для analytics/workflow cards hover-depth and spacing overrides: добавлен единый hover-depth baseline для `workflowStageCard`/`kpiCard` и убрано пересечение с глобальной premium shadow веткой.
4. Расширен smoke coverage для analytics fallback source mapping + dialog export empty/error branches (`SearchWorkflowAnalyticsDialogUseCase`, `SearchAnalyticsDialogExportFlowUseCase`) в `scripts/unit-smoke.js`.

### Wave 30 — завершено
1. Продолжена декомпозиция `Search.controller`: выделен `SearchExportOrchestrationUseCase` для export menu orchestration + user feedback/toast policy.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-export-lifecycle.py` для export lifecycle (menu default/specific entity → empty/success/error branches) с runtime assertions по routing/rows callback path и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для export/action toolbar density and button hierarchy overrides: устранено пересечение accent branches между generic `.searchToolbar` и `.brandActionBtn*`.
4. Расширен smoke coverage для export promise routing (screen/backend), empty result short-circuit и error presentation branches (`SearchAnalyticsExportUseCase`, `SearchExportOrchestrationUseCase`, `SearchAnalyticsDialogExportFlowUseCase`) в `scripts/unit-smoke.js`.

### Wave 31 — завершено
1. Продолжена декомпозиция `Detail.controller`: выделен `DetailLocationValueHelpUseCase` для location value-help lifecycle + selection propagation policy.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-location-valuehelp-flow.py` для location value-help flow (open/search/select/close) с assertions по selected basic/location sync и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для detail value-help/table hover/readability overrides: устранены пересекающиеся theme branches и объединен unified background/hover/readability блок для `locationValueHelp` table.
4. Расширен smoke coverage для location tree filtering + selection fallback branches (`DetailLocationValueHelpUseCase`) в `scripts/unit-smoke.js`.

### Wave 32 — завершено
1. Продолжена декомпозиция `Detail.controller`: выделен `DetailPersonSuggestionUseCase` для suggestion flow (observer/observed) + dictionary propagation policy.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-person-suggestion-flow.py` для person suggestion lifecycle (suggest/query/select/apply) с assertions по `OBSERVER_*`/`OBSERVED_*` field sync.
3. Проведен CSS cleanup pass для detail suggestion/value-state readability overrides с устранением пересекающихся form-field branches.
4. Расширен smoke coverage для suggestion filtering edge branches (empty query, perner fallback, duplicate position names) в `scripts/unit-smoke.js`.

### Wave 33 — завершено
1. Продолжена декомпозиция `Detail.controller`: выделен `DetailDictionarySelectionUseCase` для dictionary key/text propagation и LPC barrier-reset decision policy.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-dictionary-selection-flow.py` для dictionary selection lifecycle (resolve/apply/confirm-branch) и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для detail dictionary select/value-state readability overrides с выравниванием readable text policy для LPC/Profession селекторов.
4. Расширен smoke coverage для dictionary edge branches (`selectedItem`/fallback source key, confirm/reject barrier reset) в `scripts/unit-smoke.js`.

### Wave 34 — завершено
1. Продолжена декомпозиция `Detail.controller`: выделен `DetailLpcBarrierWarningFlowUseCase` для barrier/LPC warning orchestration (prompt adapter + decision side-effects).
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-lpc-barrier-warning-flow.py` для LPC warning flow (open → reject/confirm branches) с assertions по `/barriers` и `/basic/LPC_*` и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для warning-dialog action emphasis consistency (brand/ghost hierarchy) в message-dialog footer actions.
4. Расширен smoke coverage для negative веток warning-dialog integration (no barriers, already-allowed LPC, missing selected model hooks) в `scripts/unit-smoke.js`.

### Wave 35 — завершено
1. Продолжена декомпозиция `Detail.controller`: выделен `DetailIntegrationEditWarningUseCase` для integration-edit warning dialog orchestration (prompt + resolve policy).
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-integration-warning-flow.py` для integration warning flow (non-integration + yes/no branches) и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для consistency warning/info dialog typography spacing в detail-related message dialogs.
4. Расширен smoke coverage для integration warning negative branches (missing root, non-integration root, missing messageBox adapter) в `scripts/unit-smoke.js`.

### Wave 36 — завершено
1. Продолжена декомпозиция `Detail.controller`: выделен `DetailUnsavedDecisionFlowUseCase` для unsaved-confirmation prompt adapter (save/cancel/close decision routing).
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-unsaved-decision-flow.py` для unsaved decision flow (clean/save/discard/cancel branches) с assertions по transition policy и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для unsaved/integration dialog footer button spacing/alignment consistency.
4. Расширен smoke coverage для negative веток unsaved decision routing (missing save handler, rejected promise, no-dirty short-circuit) в `scripts/unit-smoke.js`.

### Wave 37 — завершено
1. Продолжена декомпозиция `Detail.controller`: выделен `DetailCloseNavigationFlowUseCase` для close-navigation orchestration (lock release + route intent adapter + finalize fallback).
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-detail-close-navigation-flow.py` для close-navigation flow (missing object id / release success / release error branches) с assertions по route intent/state finalize и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для detail control rail close-action consistency (spacing/hierarchy for close + edit actions).
4. Расширен smoke coverage для negative веток close flow (release error fallback, missing router adapter, no active object id) в `scripts/unit-smoke.js`.

### Wave 38 — завершено
1. Продолжена декомпозиция `Search.controller`: выделен `SearchRetryLoadPresentationUseCase` для retry/load-error presentation orchestration на adapter-уровне.
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-search-load-retry-flow.py` для search load retry flow (missing loader / empty rows / retry error / success branches) и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для search header KPI/loading/error banner consistency (spacing + visual hierarchy).
4. Расширен smoke coverage для negative веток retry/load orchestration (missing loader callback, empty rows after retry, repeated retry failures) в `scripts/unit-smoke.js`.

### Wave 39 — завершено
1. Продолжена декомпозиция `Search.controller`: выделен `SearchToolbarActionStateUseCase` для toolbar action-state orchestration (selection-dependent enable/disable + loading/smart-mode guards).
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-search-toolbar-action-state-flow.py` для toolbar action-state flow (selection + smart/fallback + loading branches) и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для search table action-toolbar density consistency (button spacing + split menu alignment).
4. Расширен smoke coverage для negative веток toolbar orchestration (missing selection context, stale selection after reload, hidden smart-controls branch) в `scripts/unit-smoke.js`.

### Wave 40 — завершено
1. Продолжена декомпозиция `Search.controller`: выделен `SearchNavigationIntentUseCase` для search navigation intent orchestration (create/copy/detail route state preparation).
2. Добавлен browser-smoke сценарий `scripts/browser-smoke-search-navigation-intent-flow.py` для search navigation intent flow (create/copy/open detail + missing router branch) и подключен в nightly/non-blocking CI lane.
3. Проведен CSS cleanup pass для search quick-filters + action toolbar alignment consistency between smart/fallback modes.
4. Расширен smoke coverage для negative веток navigation intent (missing selected id for copy, missing router adapter) в `scripts/unit-smoke.js`.

### Wave 41 — следующий этап
1. Продолжить декомпозицию `Search.controller`: вынести delete-confirmation orchestration (selection validation + backend delete + post-delete selection reset) в отдельный usecase.
2. Добавить browser-smoke сценарий search delete orchestration flow (no-selection, success delete, backend error branches) с assertions по toast/error state и selection reset.
3. Провести CSS cleanup pass для delete/copy/create action emphasis consistency в search toolbar.
4. Расширить smoke coverage для negative веток delete orchestration (missing id, stale selected model, reload promise reject).

## 5) Целевые метрики рефакторинга
- Сократить размер `Search.controller.js` до ~450-500 LOC.
- Сократить размер `Detail.controller.js` до ~700-750 LOC.
- Довести покрытие unit-тестов util/usecase до baseline 60%+ по критическим веткам.
