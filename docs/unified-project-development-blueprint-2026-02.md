# Unified Project Development Blueprint (SAP Lead Architect Auditor)

> Единый документ развития проекта: архитектура + delivery roadmap + единый блок рефакторинга (controllers / UX / UI) + governance + quality gates.
> Документ предназначен как канонический ориентир для следующих execution waves.

## 1) Executive audit snapshot (current state)

### 1.1 Что уже сильно
1. **Policy-first foundation сформирован**: значимая доля workflow-логики уже в `service/usecase/*`.
2. **Deterministic contracts внедрены в критичных ветках**: save/conflict/retry/export частично нормализованы outcome-объектами.
3. **Governance baseline существует**: smoke/gate контуры и freeze/rollback документация уже в проекте.
4. **Search/Detail extraction запущен**: контроллеры движутся к thin-adapter модели.

### 1.2 Что еще ограничивает масштабирование
1. **Controller density еще выше целевого**: Search/Detail содержат остаточную orchestration и mutation glue.
2. **UX/UI consistency не полностью централизована**: часть статусов/empty/error/pending presentation еще распределена.
3. **State contracts не завершены как grouped apply-слой**: встречаются scattered `setProperty` последовательности.
4. **Real-mode readiness не доведен до release-grade lane**: нужны более строгие integration/contract gates.

---

## 2) Target Architecture (final)

### 2.1 Layering (mandatory)
- `usecase (policy)` -> `controller (adapter)` -> `view/fragment (presentation)`.
- Контроллеры не содержат бизнес-orchestration, только wiring/callback adaptation.

### 2.2 State contract model
- Групповые apply-процедуры для доменов:
  - `search/*`
  - `detail/*`
  - `lock/*`
  - `save/*`
  - `diagnostics/*`
  - `analytics/*`
- Запрещены неструктурированные/дублирующиеся мутации состояния в контроллерах.

### 2.3 Outcome taxonomy
- Для критичных flow обязательны `ok/reason/status/code`.
- Запрещены «немые» fallback ветки и неоднозначные reason-коды.

---

## 3) Единый блок рефакторинга (Controllers + UX + UI)

## 3.1 Controllers refactoring program

### C1 — Search.controller full thin-adapter
**Цель:** исключить остаточную orchestration-логику и дубли state mutation.

**Scope:**
- analytics/filter-hint lifecycle sync extraction;
- retry/export/delete/create/copy/open branches consolidation;
- route/search/reset/mode toggle callbacks -> usecase wrappers.

**Done criteria:**
- Контроллер adapter-only;
- orchestration вынесена в usecase/coordinator;
- все ветки возвращают deterministic outcomes.

### C2 — Detail.controller full thin-adapter
**Цель:** завершить extraction lifecycle/lock/save/dialog/value-help/suggestion контуров.

**Scope:**
- close/unsaved/toggle-edit/save envelopes (уже начато) довести до полного покрытия;
- dictionary/value-help/suggestion orchestration extraction;
- expanded-dialog/row mutation lifecycle extraction;
- lock status/pending/dirty grouped state sync.

**Done criteria:**
- Контроллеры не содержат policy branching;
- grouped state sync применен для detail lifecycle;
- lock/save/conflict flow полностью deterministic.

### C3 — Component/bootstrap decomposition
**Цель:** разделить bootstrap concerns на composable orchestration services.

**Scope:**
- startup diagnostics;
- metadata readiness/degraded governance;
- lifecycle wiring isolation (без «god component» паттернов).

**Done criteria:**
- `Component.js` содержит только composition/wiring;
- startup contracts покрыты smoke tests.

---

## 3.2 UX refactoring program

### U1 — Unified status/pending/error language
- Единая модель статусов для Search/Detail:
  - pending,
  - success,
  - degraded,
  - error,
  - read-only safe mode.

### U2 — Deterministic empty/error/filter-hint experience
- Консолидация empty-state presentation, filter-hint логики и load-error поведения.
- Smart/fallback parity по визуальному и поведенческому контракту.

### U3 — Conflict and retry UX hardening
- Унификация пользовательских сообщений и веток решений (reload/overwrite/cancel).
- Гарантия одинаковых outcomes в toast/banner/state paths.

---

## 3.3 UI refactoring program

### UI1 — Fragment-first presentation reuse
- Все повторяющиеся visual/state блоки вынести в fragments.
- Убрать дубли presentation logic из controller callbacks.

### UI2 — Visual contract consistency
- Привести status-strip hierarchy к единой структуре (hint/degraded/error).
- Поддержать responsive consistency для ключевых layout зон (search rail/detail sections).

### UI3 — Accessibility and semantics pass
- Проверить семантику и доступность ключевых controls/labels/states.
- Устранить разрывы между state и видимой индикацией.

---

## 4) Delivery roadmap (single consolidated)

### Wave R1 — Controller completion
- Закрыть C1 + C2 критичные остатки.
- Закрепить thin-adapter профиль smoke/gate проверками.

### Wave R2 — State/outcome normalization
- Ввести grouped state apply для Search/Detail/lock/save.
- Завершить deterministic outcome taxonomy для всех критичных flow.

### Wave R3 — UX/UI consistency
- Закрыть U1/U2/U3 + UI1/UI2/UI3.
- Зафиксировать visual/state contract в документации.

### Wave R4 — Integration/governance hardening
- Усилить real-mode integration lane и contract diagnostics.
- Закрепить ADR/change-log/freeze/rollback как обязательный release контур.

---

## 5) Quality gates and verification matrix

## 5.1 Mandatory checks
1. `node scripts/unit-smoke.js`
2. `node scripts/unit-smoke.js --json > /tmp/unit-smoke-report.json`
3. `node scripts/ci-smoke-gate.js /tmp/unit-smoke-report.json`
4. `node scripts/wave-c-regression-gate.js /tmp/unit-smoke-report.json`
5. `node scripts/wave-d-regression-gate.js /tmp/unit-smoke-report.json`
6. `node scripts/wave-e-release-governance-gate.js`

### 5.2 Browser smoke (если среда позволяет)
- detail lock lifecycle
- detail save conflict
- detail dictionary/value-help
- search critical navigation/selection/analytics sync

### 5.3 Regression policy
- Любой refactor change mergeable только при green mandatory gates.
- Любая environment limitation документируется явно (без маскировки).

---

## 6) KPI & success criteria

### 6.1 Engineering KPIs
- Снижение controller orchestration density wave-by-wave.
- Рост доли usecase-covered критичных flow.
- Снижение unknown/ambiguous outcome branches до 0 в критичном контуре.

### 6.2 Product/UX KPIs
- Zero-regression lock lifecycle.
- Predictable save/retry/conflict resolution UX.
- Smart/fallback UX parity без деградаций.

### 6.3 Governance KPIs
- 100% docs/code parity по крупным refactor waves.
- ADR/change-log записи на каждый значимый архитектурный пакет.

---

## 7) Risks and mitigation

1. **Риск:** незавершенная extraction-инерция (частично вынесенные ветки).
   - **Mitigation:** доводить extraction пакетами с явным DoD и gate coverage.

2. **Риск:** расхождение docs и фактического кода.
   - **Mitigation:** обязательный doc update contract в каждом PR.

3. **Риск:** environment gaps для browser-smoke.
   - **Mitigation:** фиксировать как limitation + держать unit/gate контур строгим.

4. **Риск:** real-mode контрактная неоднозначность.
   - **Mitigation:** расширять diagnostics matrix и release blocking lane.

---

## 8) Execution protocol (autonomous)

1. План 12–20 шагов на wave.
2. Выполнение без ручных подтверждений между шагами.
3. После каждого шага: code/test/docs sync.
4. При блокере: root cause + fallback + продолжение незаблокированных задач.
5. Каждый wave обязан включать pair-delivery: code refactor + CSS/style consistency pass для того же блока.
6. Финал wave: commit + PR с change->artifact->test + risks + rollback.

---

## 9) Final architect verdict

Проект имеет сильный enterprise foundation и демонстрирует правильный вектор рефакторинга.
Для достижения полного целевого состояния необходима финализация controller thin-adapter профиля, полная нормализация state/outcome контрактов и завершение UX/UI consistency программы с жесткими quality gates.

**Текущая оценка:** высокий прогресс, но не final-complete.
**Цель после закрытия roadmap R1–R4:** production-grade refactor-ready состояние.
