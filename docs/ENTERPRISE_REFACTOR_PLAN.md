# Enterprise Audit & Refactoring Plan

## 1) Executive audit (Tech Lead Architect / Auditor view)

### Current strengths
- Clear domain split between UI (`controller`, `view`), orchestration (`service/usecase`) and adapters (`service/backend`).
- Existing smoke/regression scripts in `scripts/` and backend simulation in `mock_gate_way/`.
- i18n and routing are present, and the app has explicit state models in `manifest.json`.

### Main growth zones (enterprise gaps)

#### Functionality
- Very large controller surfaces (`Search.controller.js`, `Detail.controller.js`) create delivery risk: difficult onboarding, expensive change impact, and slower defect isolation.
- Missing formal module boundaries for domain capabilities (Search, Detail, Analytics, Locking, Export, Dictionary).
- No explicit capability matrix/versioning contract between UI and backend adapters.

#### UX/UI
- No centralized enterprise design-system compliance checklist (states, accessibility, consistency, interaction latency budgets).
- UX acceptance criteria are not encoded into automated gates (empty states, retries, conflict UX, progressive disclosure).
- No documented UX telemetry KPI policy (task completion time, user friction, retry frequency).

#### Architecture / Platform
- Architecture quality metrics exist, but were informational only; no non-regression gate enforcing architectural guardrails.
- No staged refactoring backlog with traceable completion statuses and acceptance criteria.
- No explicit release readiness checklist combining architecture + quality + security + UX metrics.

## 2) Full enterprise refactor backlog (all required blocks)

> Legend: `[x] done in this iteration`, `[~] started`, `[ ] planned`.

### A. Architecture & modularity
- [x] A1. Define enterprise refactor program document with measurable acceptance criteria per block.
- [x] A2. Introduce architecture non-regression gate (controller size / module growth / utility growth guardrails).
- [~] A3. Split `Search.controller.js` into feature coordinators + presentation adapters (search trigger/rebind + selection/open interaction orchestration + filter interaction policy orchestration + retry-load orchestration + export-intent + export-execution orchestration + inline analytics rail + auto-refresh + workflow-analytics dialog open/close + load orchestration blocks extracted).
- [~] A4. Split `Detail.controller.js` into edit lifecycle, save lifecycle, dictionary/location adapters (save-error outcome lifecycle KPI/presentation envelope + location value-help lifecycle envelope + dictionary selection lifecycle envelope extracted).
- [ ] A5. Introduce bounded contexts at code level (`domain/search`, `domain/detail`, `domain/lock`, `domain/analytics`).
- [ ] A6. Add architecture decision records (ADR) for backend mode strategy, lock strategy, and orchestration style.

### B. Functional resilience
- [x] B1. Formalize API capability contract + semantic version policy (policy-range validation + fail-fast startup enforcement + gate + tests delivered).
- [x] B2. Introduce contract tests between UI adapter and fake/real backend modes (fixtures + contract-drift gate + smoke parity).
- [x] B3. Add idempotency and conflict-resolution test suite for save/lock flows (deterministic matrix and no-duplicate-side-effect checks).

### C. UX/UI enterprise readiness
- [x] C1. Create design governance checklist (loading, empty, error, conflict, permission states).
- [x] C2. Define accessibility baseline (keyboard flows, focus order, aria, contrast checks).
- [x] C3. Add UX latency SLOs and user-journey telemetry dashboards.
- [x] C4. Standardize message catalog tone and severity taxonomy.

### D. Security & compliance
- [ ] D1. Threat model for lock/save/export/data lookup flows.
- [ ] D2. Centralized security headers/CSP strategy for deployment topology.
- [ ] D3. Add dependency/license audit and SBOM generation in CI.

### E. Quality engineering
- [x] E1. Add architecture metrics gate script for CI/pre-push usage.
- [ ] E2. Expand deterministic unit tests per use case cluster.
- [ ] E3. Add end-to-end risk-based suite with release gates (P0/P1 scenarios).
- [ ] E4. Introduce mutation/static analysis gates and failure budget policy.

### F. Delivery/operations
- [ ] F1. Define release readiness checklist with Go/No-Go criteria.
- [ ] F2. Add rollout strategy (canary/feature-flag) and rollback playbook.
- [ ] F3. Add runbooks for lock incidents, backend degradation, and data mismatch.

### G. Data & observability
- [ ] G1. Define canonical event schema for analytics.
- [ ] G2. Add distributed correlation IDs and traceability from UI action to backend response.
- [ ] G3. Add product KPI dashboard aligned to enterprise objectives.

## 3) What was implemented now (execution of required work from this list)

### Completed now
4. **C1-C4 UX enterprise readiness delivered** via design governance checklist + state coverage baseline + a11y/reduced-motion gate + UX SLO telemetry/report/runbook + message taxonomy metadata/lint gates.
1. **Refactor program baseline created** (`A1`) in this file with full block inventory and success criteria.
2. **Architecture readiness gate implemented** (`A2`, `E1`) via:
   - `scripts/enterprise-readiness-gate.js` (enforces non-regression thresholds).
   - `scripts/enterprise-readiness-thresholds.json` (single source of guardrail thresholds).
3. **Pre-push quality flow upgraded** to run both unit smoke and enterprise architecture gate.

### Acceptance criteria for this iteration
- Architecture gate fails when controller/module growth exceeds accepted threshold.
- Pre-push script enforces the new gate automatically.
- Refactor plan exists as a trackable enterprise backlog with status markers.

## 4) Next recommended wave (2-4 weeks)
- Decompose Search controller into 3-5 coordinators with preserved behavior.
- Decompose Detail controller into lifecycle-specific services.
- Add ADR set and contract tests for backend adapter modes.
- Roll out accessibility and UX quality gates in CI.

## 5) Completion framework: full steps/etapes/blocks per each backlog item

> Purpose: a point is considered **completed** only when all implementation steps and DoD checks are closed.

### A. Architecture & modularity

#### A1. Enterprise refactor program document
**Steps / stages / blocks**
1. Inventory all capability domains and technical layers.
2. Define target-state architecture map (current vs target).
3. Map initiatives to business risk and delivery impact.
4. Add measurable KPIs for each initiative (lead time, defect rate, MTTR, UX success rate).
5. Add governance model (owners, cadence, reporting).

**Definition of Done (DoD)**
- Program document approved by architecture + product owners.
- Each initiative has owner, priority, timeline and measurable outcome.
- Quarterly review cadence is scheduled.

#### A2. Architecture non-regression gate
**Steps / stages / blocks**
1. Select architecture health metrics (controller size, module growth, complexity trend).
2. Define thresholds by team capacity and maintainability target.
3. Implement automated checker and fail-fast behavior.
4. Integrate gate into local and CI pipelines.
5. Define exception workflow with expiry date for temporary waivers.

**DoD**
- Gate runs in CI and pre-push by default.
- Violations block merge.
- Waiver process exists and is auditable.

#### A3. Decompose Search.controller
**Steps / stages / blocks**
1. Slice controller by responsibilities (routing, filters, table lifecycle, analytics, export).
2. Create feature coordinators/use-case adapters per slice.
3. Move pure transformations to utility/domain services.
4. Keep controller as composition root only.
5. Add regression tests for extracted flows.

**DoD**
- Controller reduced to agreed max size and orchestration role.
- All migrated flows covered by tests.
- No behavior change in smoke/E2E scenarios.

#### A4. Decompose Detail.controller
**Steps / stages / blocks**
1. Split edit lifecycle, save lifecycle, lock lifecycle, dictionary/location handling.
2. Introduce dedicated orchestration services for save/lock conflict branches.
3. Extract UI state computations to dedicated presenters.
4. Remove duplicated state mutations.
5. Validate with risk-based integration tests.

**DoD**
- Detail controller reduced to orchestration shell.
- Save/lock/dictionary branches are independently testable.
- Conflict and recovery paths pass non-regression suite.

#### A5. Introduce bounded contexts
**Steps / stages / blocks**
1. Define context boundaries and ubiquitous language.
2. Move modules into context folders and enforce import rules.
3. Add anti-corruption layer between UI orchestration and backend contracts.
4. Publish context ownership map.

**DoD**
- Context boundaries documented and enforced (lint/import rules).
- Cross-context dependencies are explicit and minimized.
- Ownership assigned per context.

#### A6. ADR set
**Steps / stages / blocks**
1. Create ADR template and numbering convention.
2. Capture decisions for backend mode, lock model, orchestration style, caching strategy.
3. Record alternatives and trade-offs.
4. Link ADRs to code artifacts and tests.

**DoD**
- Required ADR list published and reviewed.
- All major architectural choices are traceable.
- New architectural changes require ADR update.

### B. Functional resilience

#### B1. API capability contract + semver
**Steps / stages / blocks**
1. Define capability matrix and mandatory/optional fields.
2. Version contract with compatibility rules.
3. Implement negotiation/fallback strategy in adapter.
4. Document deprecation and sunset process.

**DoD**
- Versioned contract published.
- Backward compatibility checks are automated.
- Unsupported capabilities degrade gracefully.

#### B2. Contract tests (fake/real backend)
**Steps / stages / blocks**
1. Define golden request/response fixtures.
2. Run same contract suite against fake and real adapters.
3. Add strict schema assertions and semantic assertions.
4. Add CI gate on contract drift.

**DoD**
- Contract suite is mandatory in CI.
- Drift is detected before merge.
- Adapter parity report is generated.

#### B3. Idempotency/conflict tests
**Steps / stages / blocks**
1. Enumerate conflict scenarios (lock kill, stale etag, partial save).
2. Add deterministic test harness for retries and duplicate submissions.
3. Validate user-visible outcomes/messages.
4. Validate data consistency after retries.

**DoD**
- Idempotency scenarios are covered by automated tests.
- No duplicate side effects on repeated requests.
- Conflict recovery flow is deterministic.

### C. UX/UI enterprise readiness

#### C1. Design governance checklist
**Steps / stages / blocks**
1. Define required states per screen (loading/empty/error/conflict/permission).
2. Define component-level consistency rules (spacing, typography, controls).
3. Add review checklist to design + code review process.
4. Add visual regression references for critical screens.

**DoD**
- Checklist is mandatory in PR reviews.
- All critical screens have state coverage.
- Visual regressions are detectable.

#### C2. Accessibility baseline
**Steps / stages / blocks**
1. Define WCAG target level and UI5 accessibility profile.
2. Audit keyboard navigation and focus order.
3. Validate labels/ARIA for controls and dialogs.
4. Run automated a11y tooling plus manual checks.

**DoD**
- A11y violations tracked to zero critical/high.
- Keyboard-only critical journeys are usable.
- Accessibility gate added to CI.

#### C3. UX latency SLO + telemetry
**Steps / stages / blocks**
1. Define user-journey SLOs (search latency, save feedback, dialog response).
2. Instrument client timings and backend dependency timings.
3. Build dashboards and alert thresholds.
4. Add periodic SLO review with product/engineering.

**DoD**
- SLOs documented and measurable.
- Dashboard and alerts operational.
- Breach handling playbook exists.

#### C4. Message taxonomy
**Steps / stages / blocks**
1. Define severity taxonomy and writing guidelines.
2. Normalize message keys and ownership.
3. Review i18n catalogs for consistency and clarity.
4. Add lint/check script for message metadata completeness.

**DoD**
- Message catalog follows unified taxonomy.
- User-facing tone is consistent across modules.
- Missing/invalid message metadata is CI-blocking.

### D. Security & compliance

#### D1. Threat model
**Steps / stages / blocks**
1. Build data-flow diagram for lock/save/export/lookups.
2. Identify threats (spoofing, tampering, repudiation, info disclosure, DoS).
3. Assign mitigations and residual risk owners.
4. Validate controls via security tests.

**DoD**
- Threat model documented and approved.
- High-risk items mitigated or formally accepted.
- Security tests cover critical abuse cases.

#### D2. Headers/CSP strategy
**Steps / stages / blocks**
1. Define deployment topologies and trust boundaries.
2. Specify CSP, HSTS, X-Frame-Options, Referrer-Policy and cookie policies.
3. Test policies in dev/stage/prod equivalents.
4. Add policy validation in deployment pipeline.

**DoD**
- Security headers baseline enforced in all environments.
- CSP violations monitored.
- Exceptions are documented and time-bound.

#### D3. Dependency/license audit + SBOM
**Steps / stages / blocks**
1. Integrate dependency vulnerability scanner.
2. Integrate license compliance scanner.
3. Generate SBOM per build artifact.
4. Define remediation SLA by severity.

**DoD**
- Build fails on policy-violating vulnerabilities/licenses.
- SBOM generated and stored per release.
- SLA dashboard for remediation is active.

### E. Quality engineering

#### E1. Architecture gate in CI
**Steps / stages / blocks**
1. Maintain metric set and thresholds.
2. Ensure gate executes in developer and CI workflows.
3. Track trend and threshold calibrations.

**DoD**
- Gate is mandatory and stable.
- Trend reports reviewed each iteration.

#### E2. Deterministic unit coverage expansion
**Steps / stages / blocks**
1. Group use-cases by risk and business criticality.
2. Add positive/negative/boundary tests.
3. Remove flaky dependencies with test doubles.
4. Enforce minimum coverage by module.

**DoD**
- P0/P1 use-cases have deterministic test coverage.
- Flakiness is below agreed threshold.
- Coverage targets enforced.

#### E3. Risk-based E2E suite
**Steps / stages / blocks**
1. Identify critical user journeys and failure modes.
2. Implement stable E2E scripts with deterministic test data.
3. Tag suites by release criticality (blocking/non-blocking).
4. Add release gate and trend reports.

**DoD**
- Critical journey E2E is release-blocking.
- E2E runtime and pass-rate are within SLO.

#### E4. Mutation/static analysis + failure budget
**Steps / stages / blocks**
1. Introduce static analysis and complexity checks.
2. Pilot mutation testing for critical modules.
3. Define quality failure budget and escalation path.
4. Integrate into engineering scorecard.

**DoD**
- Static analysis baseline has no critical issues.
- Mutation score target achieved for critical modules.
- Failure budget governance active.

### F. Delivery/operations

#### F1. Release readiness checklist
**Steps / stages / blocks**
1. Build unified Go/No-Go checklist (arch, QE, security, UX, ops).
2. Define mandatory sign-offs.
3. Automate evidence collection from CI dashboards.

**DoD**
- Release cannot proceed without checklist completion.
- Sign-offs and evidence are archived.

#### F2. Rollout and rollback strategy
**Steps / stages / blocks**
1. Define feature-flag/canary model.
2. Define health KPIs and rollback triggers.
3. Create one-click rollback playbook.
4. Rehearse rollout/rollback drills.

**DoD**
- Controlled progressive rollout available.
- Rollback tested and time-bounded.

#### F3. Operational runbooks
**Steps / stages / blocks**
1. Create runbooks for lock incidents, backend degradation, data mismatch.
2. Define diagnosis tree, mitigation steps and escalation matrix.
3. Add ownership and on-call handoff rules.
4. Run incident simulation exercises.

**DoD**
- Runbooks are actionable and validated by drills.
- MTTR target is measurable and improving.

### G. Data & observability

#### G1. Canonical event schema
**Steps / stages / blocks**
1. Define event naming/versioning conventions.
2. Define required fields (context, actor, correlation, outcome).
3. Validate event producers and consumers with schema checks.
4. Add data quality monitoring.

**DoD**
- Event schema is versioned and governed.
- Schema validation is automated.
- Data quality KPIs are green.

#### G2. Correlation IDs and traceability
**Steps / stages / blocks**
1. Generate correlation IDs at UI action start.
2. Propagate ID through adapters and backend requests.
3. Log and expose ID in observability tools.
4. Add troubleshooting workflow by correlation ID.

**DoD**
- End-to-end request tracing works for critical flows.
- Incidents can be diagnosed by correlation ID.

#### G3. Product KPI dashboards
**Steps / stages / blocks**
1. Define KPI tree (adoption, success rate, cycle time, errors, rework).
2. Build dashboard views by role (product, engineering, operations).
3. Set targets and alert thresholds.
4. Integrate KPI review into planning cadence.

**DoD**
- KPI dashboards are live and used in decision-making.
- KPI targets are tracked with accountable owners.

## 6) Legacy docs consolidation (re-validated and integrated)

Ниже — перечень актуальных пунктов, перенесённых из старых аудитов/планов оптимизации в единый enterprise-план.

### 6.1 Integrated from historical audits/plans

#### Search UX/performance hardening (integrated)
- Автозаполнение `visibleCheckLists` после initial load (без «пустой таблицы до Search»).
- `Copy` работает от выбранной записи, а не от первого элемента списка.
- Для больших списков: growing/paging strategy + лимиты initial render.
- Для ошибок загрузки: action-oriented recovery (Retry + CTA).
- Для фильтров: явные labels/aria и keyboard-friendly фокусные переходы.

**Mapped backlog items**: C1, C2, C3, E3.

#### SAP/contract alignment (integrated)
- Backend capability negotiation и strict version policy.
- Contract parity fake/real backend adapters.
- Lock/session lifecycle as deterministic policy (killed/release/retry).
- ADR-фиксация выбранных архитектурных trade-offs.

**Mapped backlog items**: A6, B1, B2, B3, D1.

#### Component/controller decomposition (integrated)
- "God component" decomposition на bootstrap orchestrators.
- Thin-adapter pattern для Search/Detail controllers.
- Unified outcome taxonomy for async outcomes/errors.

**Mapped backlog items**: A3, A4, A5, E2.

#### Delivery and release governance (integrated)
- Единый Go/No-Go checklist.
- Canary/feature-flag rollout + rollback drills.
- Runbooks для lock/backend/data mismatch incidents.

**Mapped backlog items**: F1, F2, F3.

#### Compliance and observability (integrated)
- Correlation IDs end-to-end.
- Event schema governance и data quality checks.
- KPI dashboards + SLO-based operational reviews.

**Mapped backlog items**: G1, G2, G3, C3.

### 6.2 New mandatory micro-items added after legacy review

> Эти пункты обязательны для формального enterprise-close и были выявлены при повторной сверке старых документов.

- [ ] H1. Full i18n completeness: удалить hardcoded UI-строки, ввести lint на незарегистрированные message keys.
- [ ] H2. Search initial state correctness: исключить пустой стартовый список при наличии данных.
- [ ] H3. Search table scalability: controlled growing/paging + performance budget.
- [ ] H4. Copy action correctness: копирование только явного user selection с UX-confirmation.
- [ ] H5. Unified async outcome taxonomy: success/retry/conflict/network/degraded.
- [ ] H6. Timezone/date compliance baseline for frontend/backend contracts.
- [ ] H7. Documentation governance: один master-plan + controlled set of operational docs.

## 7) Detailed execution clarifications for new items (H1–H7)

### H1. Full i18n completeness
**Implementation blocks**
1. Скан кода на hardcoded пользовательские строки (views/fragments/controllers).
2. Перенос строк в i18n bundles с RU/EN fallback policy.
3. Добавление check-script (CI) на hardcoded literals в UI слое.
4. Ревью tone-of-voice и severity alignment.

**DoD**
- 0 критичных hardcoded UI строк в контролируемых scope.
- CI-проверка блокирует новые нарушения.

### H2. Search initial state correctness
**Implementation blocks**
1. Зафиксировать expected state chart для initial load.
2. Синхронизировать `checkLists` → `visibleCheckLists` на стартовом success path.
3. Добавить negative/edge tests (empty payload, partial payload, error recovery).
4. Проверить UX поведение в smoke flow.

**DoD**
- При наличии данных стартовый список видим без ручного Search.
- Тесты покрывают success/empty/error.

### H3. Search table scalability
**Implementation blocks**
1. Ввести growing/paging режим и ограничение initial render.
2. Настроить thresholds и lazy hydration стратегию.
3. Измерить render/input latency до/после.
4. Ввести perf-budget alerts.

**DoD**
- Performance budget соблюдается на целевых сценариях.
- Table interactions стабильны на large dataset.

### H4. Copy action correctness
**Implementation blocks**
1. Привязать copy строго к selected row.
2. Добавить guard и user-feedback для невыбранного состояния.
3. Добавить unit/integration тесты copy workflow.

**DoD**
- Copy deterministic и предсказуем для пользователя.
- Ошибочные ветки имеют понятный UX.

### H5. Unified async outcome taxonomy
**Implementation blocks**
1. Ввести единый enum outcome для async операций.
2. Маппинг backend ошибок на taxonomy категории.
3. Упорядочить UI messages/strip/dialog behavior.
4. Зафиксировать taxonomy в ADR + test contracts.

**DoD**
- Async outcomes интерпретируются единообразно.
- Нет "тихих" и неоднозначных fallback-веток.

### H6. Timezone/date compliance baseline
**Implementation blocks**
1. Утвердить canonical date-time contract (timezone, serialization, parsing).
2. Привести frontend format/parse к canonical contract.
3. Добавить тесты на DST, timezone offsets, locale rendering.
4. Документировать cross-system compatibility rules.

**DoD**
- Date/time roundtrip корректен в целевых timezone сценариях.
- Regression tests покрывают DST/timezone риски.

### H7. Documentation governance
**Implementation blocks**
1. Назначить `docs/ENTERPRISE_REFACTOR_PLAN.md` master-source.
2. Сократить legacy-дубли и оставить только operational документы.
3. Определить policy обновления docs (owner + review cadence).
4. Добавить changelog правило: каждое wave-изменение отражается в master-plan.

**DoD**
- Нет конкурирующих/противоречивых master-планов.
- Документация поддерживается по формальному процессу.

## 8) Documentation set after cleanup (single source governance)

### Master plan
- `docs/ENTERPRISE_REFACTOR_PLAN.md` — единственный master-plan и источник статуса программы.

### Retained operational docs
- `docs/backend-target.md` — target backend profile и выравнивание fake/real contract.
- `docs/frontend-backend-integration.md` — текущий integration-контур и ограничения.
- `docs/refactor-change-log.md` — wave-by-wave история изменений.
- `docs/rollback-playbook-search-detail.md` — критический rollback runbook.
- `docs/adr/ADR-TEMPLATE.md` — шаблон архитектурных решений.

### Governance rule
- Любые новые стратегические планы добавляются только как разделы в master-plan.
- Отдельные документы допускаются только для operational artifacts (runbook/ADR/template/integration contract).

## 9) Structural reconciliation with current repository (as-is → target)

Цель секции: исключить рассинхрон плана с реальной кодовой базой и задать точные зоны изменения.

### 9.1 Current structure checkpoints (verified)
- UI composition root: `Component.js`.
- Main orchestration controllers: `controller/Search.controller.js`, `controller/Detail.controller.js`.
- Application use-cases: `service/usecase/*.js`.
- Backend adapters: `service/backend/*.js`.
- Presentation and helpers: `util/*.js`, `manager/*.js`, `domain/*.js`.
- UI layers: `view/*.xml`, `view/fragment/*.xml`, `css/style.css`, `i18n/*.properties`.
- Supporting docs (operational): `docs/backend-target.md`, `docs/frontend-backend-integration.md`, `docs/refactor-change-log.md`, `docs/rollback-playbook-search-detail.md`, `docs/adr/ADR-TEMPLATE.md`.

### 9.2 Explicit mapping of backlog items to real code areas

#### A — Architecture & modularity
- A3 (`Search.controller` decomposition):
  - Source: `controller/Search.controller.js`.
  - Targets: `service/usecase/*Search*`, `util/Search*`, potential `domain/search/*`.
- A4 (`Detail.controller` decomposition):
  - Source: `controller/Detail.controller.js`.
  - Targets: `service/usecase/*Detail*`, `util/Checklist*`, potential `domain/detail/*`.
- A5 (bounded contexts):
  - Source: `service/usecase`, `util`, `domain`, `service/backend`.
  - Target directories: `domain/search`, `domain/detail`, `domain/lock`, `domain/analytics`.
- A6 (ADR):
  - Artifacts: `docs/adr/*.md` + cross-links into changed code files.

#### B — Functional resilience
- B1/B2/B3:
  - Adapter and contracts: `service/backend/BackendAdapter.js`, `service/backend/RealBackendService.js`, `service/backend/FakeBackendService.js`, `service/backend/FakeODataService.js`.
  - Contract fixtures/tests: `scripts/*`, unit smoke extensions.

#### C — UX/UI
- C1/C2/C4:
  - Views/fragments/i18n: `view/`, `view/fragment/`, `i18n/`.
- C3:
  - Telemetry touchpoints: controllers + `service/usecase/OperationalKpiInstrumentationUseCase.js` + analytics use cases.

#### D — Security/compliance
- D1/D2/D3:
  - Frontend policy docs and scripts + `sap_backend/src/*` (for backend-side alignment).

#### E — Quality engineering
- E1 already in `scripts/enterprise-readiness-gate.js`.
- E2/E3/E4:
  - `scripts/unit-smoke.js`, `scripts/ci-smoke-gate.js`, browser smoke scripts in `scripts/browser-smoke-*.py`.

#### F — Delivery/operations
- F1/F2/F3:
  - `docs/refactor-change-log.md`, `docs/rollback-playbook-search-detail.md`, release governance scripts in `scripts/wave-*.js`.

#### G — Data/observability
- G1/G2/G3:
  - Analytics/event logic in use-cases + backend analytics endpoints (`mock_gate_way/api/analytics_api.py`).

### 9.3 Conflict-prevention rules (mandatory)
1. Нельзя одновременно менять domain rules в контроллере и в use-case (single policy owner).
2. Любая новая async ветка обязана быть описана в unified outcome taxonomy (H5).
3. Любое backend-contract изменение требует обновления B1/B2 артефактов и ADR linkage.
4. Любое UX изменение состояния (loading/error/conflict/empty) требует C1 checklist pass.
5. Любое изменение дат/времени требует H6 timezone regression checks.
6. Любой новый стратегический план оформляется только как раздел этого master-документа.

## 10) Ultra-detailed execution grid (15/10 target quality)

Ниже каждый пункт имеет: **Inputs → Steps → Outputs → Gates → Dependencies → Risks/Anti-conflicts**.

### A3. Search.controller decomposition (ultra-detailed)
**Inputs**
- `controller/Search.controller.js`, related Search use-cases, smoke scripts.

**Steps**
1. Построить responsibility matrix: routing/filter/search/export/analytics/selection/lifecycle.
2. Для каждого responsibility выделить owner-module (usecase/coordinator/presenter).
3. Переместить pure logic из controller в модуль, оставить wiring.
4. Обновить вызовы и контракты (parameters/result taxonomy).
5. Добавить regression tests на каждый перенесённый responsibility.
6. Прогнать smoke + architecture gate, сравнить метрики до/после.

**Outputs**
- Тонкий Search controller, обновлённые use-cases, тесты.

**Gates**
- Controller lines ≤ threshold.
- Unit/smoke green.
- No UX regression in search/open/export flows.

**Dependencies**
- H5 taxonomy baseline.

**Risks / anti-conflicts**
- Риск: двойная логика фильтров.
- Контроль: единый source-of-truth в Search-specific use-case.

### A4. Detail.controller decomposition (ultra-detailed)
**Inputs**
- `controller/Detail.controller.js`, Detail use-cases, lock/save flows.

**Steps**
1. Разметить ветки edit/save/close/unsaved/lock/dictionary/value-help/person suggestion.
2. Вынести decision branches в orchestration use-cases.
3. Вынести UI-specific state projections в presenters.
4. Устранить дубли state mutation через single sync service.
5. Добавить integration tests на conflict/retry/recover.

**Outputs**
- Тонкий Detail controller, deterministic lifecycle branches.

**Gates**
- Conflict/save/close smoke flows green.
- Нет policy branching в controller.

**Dependencies**
- B3 conflict/idempotency suite.

**Risks / anti-conflicts**
- Риск: divergence lock behavior.
- Контроль: lock policy только в dedicated lock use-case.

### B1/B2/B3 contract and resilience bundle (ultra-detailed)
**Inputs**
- `service/backend/*`, mock gateway endpoints, smoke scripts.

**Steps**
1. Описать versioned capability matrix (required/optional/experimental).
2. Создать golden fixtures для fake/real parity.
3. Настроить contract test runner в CI.
4. Добавить idempotency matrix: duplicate save/retry/timeout/stale etag/lock killed.
5. Привязать UI outcome mapping к каждому backend failure class.

**Outputs**
- Contract spec + automated parity/idempotency gates.

**Gates**
- Contract drift blocks merge.
- Retry/idempotency tests deterministic.

**Dependencies**
- H5 taxonomy, D1 threat model.

**Risks / anti-conflicts**
- Риск: fake backend слишком либеральный.
- Контроль: strict fixtures and schema assertions.

### C1/C2/C3/C4 UX enterprise bundle (ultra-detailed)
**Inputs**
- Views/fragments/CSS/i18n/controller UX flows.

**Steps**
1. Утвердить UI states matrix для Search/Detail.
2. Довести a11y baseline: labels, aria, keyboard path, focus return.
3. Унифицировать text severity taxonomy в i18n.
4. Добавить SLO instrumentation (search latency, save feedback time, error recovery time).
5. Добавить visual + interaction regression checks для critical screens.

**Outputs**
- Predictable UX contract + measurable UX SLOs.

**Gates**
- A11y high/critical = 0.
- UX SLO metrics collected and reviewed.

**Dependencies**
- H1/H2/H3/H4.

**Risks / anti-conflicts**
- Риск: стиль конфликтует с perf constraints.
- Контроль: perf-budget и reduced-effects profile.

### D1/D2/D3 security and compliance bundle (ultra-detailed)
**Inputs**
- Frontend/backend integration surfaces, deployment assumptions.

**Steps**
1. STRIDE threat model per critical flow.
2. Security headers/CSP policy by environment.
3. Dependency/license scan + SBOM in CI.
4. Severity-based remediation SLA and exception workflow.

**Outputs**
- Auditable security baseline.

**Gates**
- Critical security issues block release.
- SBOM and license compliance generated per release.

**Dependencies**
- F1 release checklist.

**Risks / anti-conflicts**
- Риск: over-restrictive CSP ломает UI.
- Контроль: staged rollout with violation telemetry.

### E2/E3/E4 quality scaling bundle (ultra-detailed)
**Inputs**
- Unit smoke suite + browser smoke scripts.

**Steps**
1. Расставить P0/P1 risk tags на use-cases.
2. Закрыть пробелы deterministic unit tests.
3. Расширить E2E matrix по критическим пользовательским путям.
4. Ввести static/mutation quality score и thresholds.

**Outputs**
- Stable quality gates with failure-budget governance.

**Gates**
- P0 tests release-blocking.
- Flakiness below target.

**Dependencies**
- A3/A4 extraction завершены частично/полностью.

**Risks / anti-conflicts**
- Риск: рост времени CI.
- Контроль: parallelized suites + tiered gates.

### F1/F2/F3 operational excellence bundle (ultra-detailed)
**Inputs**
- Release docs, rollback playbook, wave scripts.

**Steps**
1. Собрать единый Go/No-Go checklist с обязательными evidence links.
2. Формализовать progressive rollout + rollback triggers.
3. Актуализировать инцидентные runbooks и провести drills.
4. Внедрить post-release review template.

**Outputs**
- Predictable release operations and incident readiness.

**Gates**
- Нет релиза без signed checklist.
- Rollback drills completed and timestamped.

**Dependencies**
- D and E bundles minimum readiness.

**Risks / anti-conflicts**
- Риск: формальный чеклист без реальных доказательств.
- Контроль: machine-collected evidence from CI/observability.

### G1/G2/G3 observability and KPI bundle (ultra-detailed)
**Inputs**
- Analytics use-cases + backend analytics endpoints.

**Steps**
1. Утвердить event schema and versioning contract.
2. Протянуть correlation ID через UI→adapter→backend.
3. Настроить dashboards по product/engineering/ops ролям.
4. Добавить alerting и monthly KPI governance review.

**Outputs**
- End-to-end traceability and KPI-driven delivery.

**Gates**
- Critical flows traceable by correlation ID.
- KPI ownership assigned and reviewed.

**Dependencies**
- C3 instrumentation, B1 contract formalization.

**Risks / anti-conflicts**
- Риск: noisy telemetry.
- Контроль: canonical schema + sampling rules.

## 11) Dependency graph and no-conflict sequence

### 11.1 Mandatory order
1. Foundation: A6 + H5 + H7.
2. Contract/security base: B1 + D1.
3. Controller decomposition: A3/A4.
4. Resilience + UX hardening: B2/B3 + C1/C2/C4 + H1/H2/H4.
5. Performance and telemetry: C3 + H3 + G1/G2.
6. QE scale-up: E2/E3/E4.
7. Operations/release hardening: F1/F2/F3 + D2/D3 + G3.

### 11.2 Parallelization rules
- Разрешён параллелизм: C1/C2 с E2; D2 с F1; G1 с C3.
- Запрещён параллелизм: A3 и A4 в одной и той же shared policy зоне без owner split.
- Любой cross-cutting change требует impact note в `docs/refactor-change-log.md`.

## 12) Definition of “Application 15/10” (final acceptance)

Проект считается достигшим уровня **15/10 enterprise quality**, только если одновременно выполнены условия:

1. **Architecture excellence**: A3/A4/A5/A6 complete; controllers are thin and bounded contexts enforced.
2. **Contract resilience**: B1/B2/B3 complete; no uncontrolled drift between fake/real backend.
3. **UX excellence**: C1/C2/C3/C4 + H1/H2/H3/H4 complete; measured UX SLO improvement.
4. **Security/compliance**: D1/D2/D3 complete with active policy enforcement.
5. **Quality at scale**: E1/E2/E3/E4 complete; flakiness and CI quality budgets within target.
6. **Operational maturity**: F1/F2/F3 complete; release and rollback are auditable and rehearsed.
7. **Observability maturity**: G1/G2/G3 + H6 complete; end-to-end tracing + KPI governance active.
8. **Documentation governance**: H7 complete; no conflicting strategic docs outside master-plan.

Если хотя бы один из блоков выше не закрыт, статус "15/10" не присваивается.


## 6) Gateway-alignment correction plan (updated with latest scope)

### Confirmed corrections from current implementation
- [x] Search now has a lightweight entity projection strategy (`SearchRows`) and routing by row `db_key`; heavy detail is loaded on demand.
- [x] Save/Autosave run through function-import style actions with request idempotency (`request_guid` + dedup ledger).
- [x] Lock observability has dedicated OData-like history entity (`LockLogs`) with `$filter/$orderby/$top/$skip`.
- [x] Runtime capabilities endpoint exists to drive UI paths without hardcoded assumptions.

### Remaining hardening tasks (next sprint)
- [ ] Add backend integration tests for lock expiry/takeover and idempotent save replay at API level.
- [ ] Add workflow tests for unsaved-decision matrix (`YES/NO/CANCEL`) including release-lock side effects.
- [ ] Add persistence policy tests:
  - Smart cache survives browser restart for UI binding data and theme.
  - Session-only datasets (dictionary/variables/hierarchy) are cleared on tab refresh.
- [ ] Add routing resilience tests for refresh/tab-close/navigation while in `EDIT` with dirty state.

### Target behavior matrix (must remain green)
1. `READ` mode -> no edit timers (heartbeat/autosave/lock-status/activity/GCD).
2. Enter `EDIT` with lock -> all lock-scoped timers start from zero.
3. Exit to `READ` (unlock) -> all lock-scoped timers stop and state resets.
4. Re-enter `EDIT` with fresh lock -> timers restart from zero (no stale carry-over).
5. Unsaved-close dialog:
   - `YES` -> save + unlock/release flow,
   - `NO` -> discard local dirty without cache overwrite + unlock/release,
   - `CANCEL` -> stay on card, no release.
6. Search list uses lightweight entity only; detail payload loads by `db_key` on demand.

### Delivery notes
- Keep Gateway-like contracts backward-compatible using fallback endpoints until migration cutover.
- Keep capability flags as single source of truth for client routing decisions.
