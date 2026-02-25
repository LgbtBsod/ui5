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

### Wave 24 — следующий этап
1. Продолжить снижение LOC `Detail.controller` через extraction detail toolbar/validation orchestration участков.
2. Добавить browser-smoke сценарий для detail save conflict UI interaction с runtime message/decision assertions.
3. Провести cleanup pass по CSS повторяющимся theme override selectors.
4. Расширить smoke coverage для backend flaky/network error recovery в search/detail pipeline.

## 5) Целевые метрики рефакторинга
- Сократить размер `Search.controller.js` до ~450-500 LOC.
- Сократить размер `Detail.controller.js` до ~700-750 LOC.
- Довести покрытие unit-тестов util/usecase до baseline 60%+ по критическим веткам.
