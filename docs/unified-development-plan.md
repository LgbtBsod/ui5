# Unified development plan (SAP Lead Architect Auditor edition)

## Цель
Свести разрозненные roadmap/plan документы в один исполнимый план, который закрывает UX/UI/workflow риски и доводит проект до production-ready состояния для SAP-ориентированного контура.

## Текущий уровень проекта (as-is audit)

### A. Уже реализованные фичи и механики (strong baseline)
1. **Search dual-mode**: SmartFilterBar/SmartTable + fallback table, action toolbar, analytics rail, filter hints, retry/error states.
2. **Detail lifecycle**: lock acquire/release, own-session steal, killed/read-only fallback, save/autosave flows, conflict handling.
3. **Data mechanics**: lazy loading checks/barriers, delta payload builder, dictionary/value-help and suggestion flows.
4. **Architecture progress**: значимая декомпозиция в `service/usecase/*`, controller-слой в целом выполняет роль adapter/orchestrator.
5. **Quality baseline**: большой smoke-unit контур (`scripts/unit-smoke.js`) и CI smoke gate script (`scripts/ci-smoke-gate.js`).

### B. Критические риски до финального результата
1. **Workflow determinism**: pre-acquire и acquire-failure ветки должны строго различаться (без лишнего recovery на pre-check сбоях).
2. **UX consistency gap**: часть pending/error copy и статусов еще не полностью стандартизована между Search и Detail.
3. **Search enterprise parity**: fallback-режим должен гарантировать эквивалент smart-режиму по action feedback и guard-политикам.
4. **Final contract readiness**: create/copy backend-led key mapping и refresh semantics требуют финальной фиксации как единого контракта.

## Product quality bar (final target)
1. **Workflow reliability**: сценарии A–H (read/open, edit/lock, autosave/fullsave, unload/release, steal/session-killed, copy/create, retry) проходят стабильно.
2. **UX consistency**: единый state-contract (`state>/isLoading`, lock pending/status, predictable empty/error states) во всех экранах.
3. **Enterprise search contract**: SmartFilterBar/SmartTable как primary контур, fallback имеет эквивалентный UX и действия.
4. **Concurrency safety**: strict freshness gate перед edit-lock acquire + conflict-safe save/retry UX.
5. **Maintainability**: повторяющийся UI вынесен во fragments/usecase-coordinators, контроллеры остаются thin-adapter.

## Consolidated requirements (best-of)

### R1 — UX/UI consistency (P0)
- Все пользовательские async-операции используют `state>/isLoading` или специализированный pending-флаг с явной индикацией.
- Error/warning/hint состояния на Search и Detail задаются из централизованных фрагментов и presentation-usecase.
- При `is_killed`/lock-loss интерфейс безусловно уходит в safe read-only с понятным статусом.

### R2 — Workflow and lock correctness (P0)
- Перед входом в edit обязательно выполняется **strict freshness check** и при необходимости backend refresh.
- Pre-check failures (freshness/integration) обрабатываются отдельно от lock-acquire conflicts/recovery.
- Lock acquire/release/steal-own-session имеют единый orchestration pipeline и не размазываются по контроллерам.
- Unsaved-decision (save/discard/cancel) стандартизован для close/toggle-edit/navigation.

### R3 — Search enterprise readiness (P1)
- Smart controls работают как основной режим; fallback повторяет ключевые действия и статусы (copy/delete/export/retry/filter hints).
- Search summary/analytics/toolbar-action-state синхронизированы через usecase orchestration.
- Empty/load-error/filter-hint состояния изолированы в переиспользуемых UI блоках.

### R4 — Performance and architecture (P1)
- Lazy load checks/barriers с first paint на root/basic.
- Usecase/coordinator extraction продолжается до устранения крупных orchestration веток из контроллеров.
- CSS поддерживается семантическими секциями без изменения visual contract.

### R5 — Quality gates (P0)
- Обязательный smoke набор: `node scripts/unit-smoke.js`, `node scripts/unit-smoke.js --json > /tmp/unit-smoke-report.json`, `node scripts/ci-smoke-gate.js /tmp/unit-smoke-report.json`.
- Browser smoke для критических flow: lock lifecycle, save conflict, search navigation/selection.
- Перед merge: no regressions по error handling, no duplicate state paths, no hidden fallback degradation.

## Delivery waves

### Wave A — Critical hardening (in progress)
1. Strict pre-edit freshness gate (done).
2. Centralized Search load-state panel (done).
3. Lock pre-check vs acquire-failure separation (done).
4. Unified lock/status UX copy for Detail pending/error/success states (done).

### Wave B — Workflow completion
1. Copy/create backend-led contract finalization (done).
2. Search fallback parity for enterprise actions (done).
3. Conflict/retry messaging polishing (completed).
4. End-to-end smoke for A–H critical user journeys (completed).

### Wave C — Final enterprise readiness
1. Smart metadata-driven Search consolidation (completed).
2. KPI instrumentation for latency/conflicts/validation failures (completed).
3. Regression gate on A–H + documentation freeze (completed).

### Wave D — Maintainability and operationalization
1. Controller slimming phase-2 (Search/Detail orchestration extraction).
2. KPI pipeline phase-2 (snapshot/export + diagnostics contract).
3. Startup capability diagnostics and deterministic degraded-mode governance.

### Wave E — Release governance and enterprise rollout
1. Real-backend nightly/pre-release integration gate.
2. ADR + architecture change-log governance.
3. Documentation freeze v2 with rollback playbooks.


## Post-Wave C strategic analysis (current project-wide snapshot)

### Current strengths
1. **Workflow determinism baseline is stabilized**: save/conflict/retry/export branches are normalized into explicit outcomes and covered by smoke matrix.
2. **Search architecture is policy-driven**: smart/fallback parity, metadata availability reason-codes, analytics mapping and retry presentation are extracted to usecases/coordinator modules.
3. **Operational observability is introduced**: `operationalKpi` counters/latency sampling are wired in save/retry/validation/conflict critical paths.
4. **Quality gates are production-oriented**: smoke json + CI gate + dedicated Wave C regression gate are in place.

### Remaining strategic gaps (after Wave C close)
1. **Controller size remains high**: Search/Detail are still orchestration-heavy for long-term maintainability.
2. **KPI lifecycle lacks persistence/export**: runtime KPI bag exists but no historical aggregation/reporting contract yet.
3. **Real-backend readiness matrix needs tightening**: fake/real capability diagnostics and release-grade integration lane should be formalized.
4. **Documentation governance**: freeze exists, but versioned ADR/change-log discipline should be added for next waves.

### Next execution waves (after Wave C)

### Wave D — Maintainability and operationalization
1. Controller slimming phase-2: extract remaining orchestration segments from Search/Detail into usecase/coordinator blocks.
2. KPI pipeline phase-2: add KPI snapshot export/usecase + diagnostics panel contract (without breaking current UX).
3. Integration diagnostics: explicit startup capability matrix (server/frontend config) with deterministic degraded-mode messaging.

### Wave E — Release governance and enterprise rollout
1. Real-backend release lane: nightly integration gate (real mode) -> pre-release blocking lane.
2. ADR/document governance: architecture decision records and refactor change-log per wave.
3. Documentation freeze v2: versioned freeze checklist + rollback playbook for Search/Detail critical flows.

## Implementation rule of this repo
- Любое UX/workflow изменение проходит через: `usecase` (policy) -> `controller` (adapter) -> `view/fragment` (presentation).
- Любые новые повторяющиеся visual state блоки сразу выносятся в fragment.
- Любая операция, меняющая lock/edit/save состояние, обязана обновлять status-индикаторы и pending-флаги.


### Active implementation block
1. Wave B.1 completed: Copy/create backend-led contract finalization (key mapping + refresh semantics).
2. Wave B.2 completed: Search fallback parity for enterprise actions and feedback consistency.
3. Wave B.2 progress: fallback selection hydration now enforces deterministic action-state sync and clears selection flag on missing id.
4. Wave B.2 progress: missing-id hydration now also clears stale selected model payload to avoid phantom selection actions in fallback mode.
5. Wave B.2 progress: export intent guard now surfaces deterministic disabled/run_error feedback to users in fallback/smart parity flows.
6. Wave B.3 completed: conflict/retry/export messaging polishing across save/retry/search branches.
7. Wave B.3 progress: save-conflict decision handler now returns deterministic outcomes (`reloaded/overwritten/cancelled/missing_*_handler`) for stable UX messaging adapters.
8. Wave B.3 progress: save error adapter now normalizes backend handler results into structured outcomes for deterministic messaging orchestration.
9. Wave B.3 progress: retry message presenter now returns deterministic `banner_only` outcome when toast adapter is absent but error banner is applied.
10. Wave B.3 progress: detail save-error outcome presentation is now normalized and mapped to deterministic UX toasts for conflict branches.
11. Wave B.3 progress: detail save-error outcome presenter now returns structured presentation results (`presented/unknown_outcome/missing_toast_adapter/toast_error`).
12. Wave B.3 progress: detail save-error adapter now normalizes backend handler rejections into `backend_error` outcome to avoid unhandled promise branches.
13. Wave B.3 progress: detail save-error outcome presenter now distinguishes `missing_bundle_adapter` vs `missing_toast_adapter` for precise diagnostics.
14. Wave B.3 progress: `backend_error` save outcomes are now mapped to `genericOperationFailed` presentation with backend error parameter for deterministic user feedback.
15. Wave B.3 progress: save-error normalization now treats resolved backend `Error` objects as `backend_error` outcomes (in addition to rejected promises).
16. Wave B.3 progress: retry outcome presenter now reports deterministic `missing_state_model_adapter` for banner branches without state adapter and separates `toast_error_banner_applied` diagnostics when toast fails after banner apply.
17. Wave B.3 progress: export intent message presenter now returns structured outcomes (`presented_disabled/presented_error/no_presentation_needed/unknown_result_reason`) and Search controller handles unknown reason with deterministic load-error fallback.
18. Wave B.3 progress: export intent presenter now distinguishes successful vs failed presentation (`presentation_failed_disabled/presentation_failed_error`) so controller can enforce deterministic fallback on toast-adapter failures.
19. Wave B.3 completed: added explicit A–H critical journeys smoke matrix in `scripts/unit-smoke.js` (read/open, edit/lock, autosave/fullsave, unload/release, recover-from-acquire, copy/create, retry, save-conflict) and validated through CI smoke gate JSON flow.
20. Wave C.1 completed: metadata-driven smart-control availability now uses consolidated reason codes (`metadata_pending/metadata_ready/metadata_error`) for deterministic Search behavior.
21. Wave C.2 completed: operational KPI instrumentation added for save/retry latency and counters (conflicts, validation failures, retry failures) via `OperationalKpiInstrumentationUseCase` wired into Detail/Search flows.
22. Wave C.3 completed: regression gate hardened with dedicated `scripts/wave-c-regression-gate.js` and documentation freeze captured in `docs/documentation-freeze-wave-c.md`.
23. Wave C completed: final enterprise readiness baseline reached (smart metadata consolidation + KPI instrumentation + regression/documentation freeze).
24. Wave D.1 started: project entered maintainability phase-2 with focus on controller orchestration extraction and explicit architectural boundaries.
25. Wave D.2 queued: KPI snapshot/export and diagnostics-panel contract definition for operational observability maturity.
26. Wave D.3 queued: startup capability diagnostics matrix for fake/real backend with deterministic degraded-mode governance.
