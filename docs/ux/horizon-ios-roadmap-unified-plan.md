# Horizon + iOS/macOS Unified Visual Transition and Execution Roadmap

Этот документ объединяет и заменяет:
- `docs/ux/horizon-ios-visual-transition-plan.md`
- `docs/ux/roadmap-audit-execution-2026-02-26.md`

Цель: чтобы у команды был **один источник правды** для визуального перехода, API/UX трассируемости, wave-планирования и release-gates.

---

## 1) North Star, scope and constraints

- Довести UI до premium-уровня в парадигме **SAP Horizon + iOS/macOS materiality**.
- Сохранить twin-theme parity: Morning+ и Night+ эквивалентны по фичам/состояниям/интеракциям.
- Удержать совместимость с текущими UX/A11Y/SLO/gate-практиками.
- Любое улучшение должно быть token-first и не ломать Search/Detail сценарии.

**Нельзя:**
- hardcoded accent в controls,
- неуправляемый reset, ломающий UI5 semantics,
- выпускать API-изменения без capability flags и contract tests.

---

## 2) Executive summary (audit-confirmed)

- Инвентаризация UI5/API/UX gate/CSS слоёв подтверждена.
- Принят обязательный twin-theme контракт Morning+ / Night+.
- Введён `horizon-color-bridge` как единый слой Horizon roles -> локальные токены.
- Для controls действует accent-role-only политика.
- Утверждены motion framework и reduced-motion fallback.
- Формализован living background (cycle/amplitude/perf caps + phase offsets).
- Зафиксированы anti-regression контуры: CSS audit, parity snapshots, a11y, SLO, semantic contrast.
- Зафиксирован rollout API-эволюции: capability flags, contract tests, migration/deprecation.

---

## 3) Style principles and design artifacts

### 3.1 Principles
1. Horizon-first discipline.
2. iOS/macOS materiality.
3. Premium calmness.
4. Functional clarity.

### 3.2 Target artifacts
- Tokens v2: `--surface-*`, `--text-*`, `--border-*`, `--semantic-*`, `--accent-*`, `--shadow-*`, `--radius-*`, `--motion-*`.
- Pattern catalog: glass card/toolbar/KPI/editable card/semantic chip/empty-error banners/dialog shell.
- Theme modes: Morning+ (air/glass), Night+ (depth/controlled glow).

### 3.3 Reset + re-skin policy
- Глобальный reset разрешён только с governance.
- Семантика `Error/Warning/Success/Information` неприкосновенна.
- Focus/hover/active/disabled — единый контракт.
- Ad-hoc overrides вне token/pattern layers запрещены.

---

## 4) Token and color bridge contract

### 4.1 Light (Morning+)
- `--surface-bg` -> S0
- `--surface-card` -> S1
- `--surface-muted` -> S4
- `--surface-selected` -> S5
- `--text-main/support/minor` -> T1/T2/T3
- `--accent-control` -> Blue 7
- `--accent-control-strong` -> Blue 8
- `--accent-decorative` -> Indigo/Teal family

### 4.2 Dark (Night+)
- `--surface-bg` -> S0 dark
- `--surface-card` -> S1 dark
- `--surface-muted` -> S4 dark
- `--surface-selected` -> S5 dark
- `--text-main/support/minor` -> T1/T2/T3 dark
- `--accent-control` -> Blue 5
- `--accent-control-strong` -> Blue 4
- `--accent-decorative` -> Teal 3

### 4.3 Semantic
- Negative, Critical, Positive, Informative, Neutral только через semantic token layer.

---

## 5) Traceability matrices (component/API/theme/risk)

### 5.1 Component matrix
- Containers (`Page`,`VBox`,`HBox`,`GridList`,`FCL`) -> единый container contract.
- Inputs (`Input`,`Select`,`DatePicker`,`TimePicker`) -> unified field contract + focus ring.
- Action controls (`Button`,`MenuButton`,`Switch`,`CheckBox`) -> accent-role-only.
- Tables (`Table`,`SmartTable`,`TreeTable`) -> smart/fallback parity.
- Dialogs/fragments (`Dialog`,`Fragment`) -> unified dialog shell + motion envelope.
- Status/feedback (`ObjectStatus`,`MessageStrip`,`BusyIndicator`) -> strict semantic map.
- DnD (`DragInfo`,`DropInfo`) -> DnD interaction contract.

### 5.2 API matrix
- Capability/metadata -> semver + contract gate.
- Search/read -> `POST /search/query` + cursor pagination (target).
- Checklist write -> merge-preview API (target).
- Locking -> realtime watch (SSE/WebSocket target).
- Runtime settings -> `/user/theme-preferences`, `/theme/palette` (target).
- Batch -> stricter conformance tests.

### 5.3 Theme parity matrix
- Default/hover/active/disabled, semantic statuses, smart/fallback states, motion envelopes, living gradient, accent controls.
- Любой gap блокирует Go до прохождения parity gate.

### 5.4 Risk matrix
- Reset ломает UI5 semantics -> reset governance + scoped overrides.
- Light/Dark drift -> parity matrix + pair snapshots.
- Accent drift -> lint/audit + no-hardcode enforcement.
- Living background perf drop -> perf caps + reduced-motion.
- OData/filter regressions -> edge contract tests.
- Smart/fallback mismatch -> mandatory dual-mode smoke.

---

## 6) Unified wave plan (0..6 + A..D)

| Wave | Scope | DoD |
|---|---|---|
| 0 | Baseline audit + screenshot packs | baseline artifacts merged |
| 1 | Tokens + CSS refactor + color bridge | duplicate/conflict CSS zones -70% |
| 2 | Living background + material upgrade | no SLO regression vs baseline |
| 3 | Card/layout air system | Search/Detail spacing parity pass |
| 3.5 | Twin parity + reset governance | 100% parity checklist pass |
| 4 | Controls premium pass | controls mapped to token roles 100% |
| 5 | Motion/micro-interactions | reduced-motion pass + UX signoff |
| 6 | Hardening + automation | CI blocks regressions on all 5 gates |
| A | API contract stabilization | golden contract tests enabled |
| B | UX-aligned API additions | new APIs behind capability flags |
| C | Realtime/observability APIs | lock watch + telemetry SLO-compliant |
| D | Rollout governance | Go/No-Go criteria enforced |

---

## 7) UI5 component roadmap (practical)

### 7.1 Current contour
- Containers: `sap.m.Page`, `sap.m.VBox`, `sap.m.HBox`, `sap.f.FlexibleColumnLayout`, `sap.f.GridList`.
- Inputs: `sap.m.Input`, `sap.m.Select`, `sap.m.DatePicker`, `sap.m.TimePicker`, `sap.ui.comp.smartfilterbar.SmartFilterBar`.
- Controls: `sap.m.Button`, `sap.m.MenuButton`, `sap.m.Switch`, `sap.m.CheckBox`.
- Data/status: `sap.m.Table`, `sap.ui.comp.smarttable.SmartTable`, `sap.ui.table.TreeTable`, `sap.m.ObjectStatus`.
- Dialog/fragment/DnD: `sap.m.Dialog`, `sap.ui.core.Fragment`, `sap.ui.core.dnd.DragInfo/DropInfo`.

### 7.2 Next candidates
- `StepInput`, `MultiInput`, `SegmentedButton`, `IconTabBar`, `Popover`, `Card`.

### 7.3 Motion by component
- Buttons/MenuButton -> micro press depth.
- Switch/CheckBox -> explicit state transition.
- Input/Select/Date/Time -> calm focus/validation transitions.
- Dialog/Popover -> uniform open/close envelope.

---

## 8) API evolution and governance

### 8.1 Current API landscape
- Core/compat: `/`, `/$metadata`, `/capabilities`, `/$batch`, `/ChecklistRoots`, `/SearchRows`.
- Checklist domain: CRUD, autosave, checks/barriers, copy, soft-delete.
- Locking: `/lock/*`, `/actions/Lock*`, lock read models.
- Reference: `/reference/bundle`, `/dict`, `/persons/suggest`, `/location`, `/hierarchy`.
- Analytics/settings: `/WorkflowAnalytics`, `/analytics/process`, `/actions/export`, `/FrontendRuntimeSettings`, `/config/frontend`.

### 8.2 Target APIs
- Search/query: `POST /search/query`, cursor pagination.
- Visual settings: `GET/PUT /user/theme-preferences`, `GET /theme/palette`.
- Observability: `POST /telemetry/ux-events`, `GET /telemetry/slo-summary`.
- Concurrency: `GET /lock/{id}/watch`, `POST /checklist/{id}/merge-preview`.
- Export jobs: async create/status/download.
- Contract governance: `/contract/changelog`, `/contract/deprecations`.

### 8.3 Mandatory API DoD
Новый endpoint не идёт в прод без:
- capability flag,
- contract tests,
- UX state coverage (loading/empty/error/conflict),
- migration/deprecation notes.

---

## 9) QA / release gates (Go/No-Go)

### Mandatory
- Morning/Night screenshot pair per touched flow.
- Smart/fallback parity pass.
- a11y pass (keyboard/focus/reduced-motion/contrast).
- smoke pass.
- semantic contrast pass.
- CSS duplicate/conflict audit pass (or time-limited waiver).
- capability compatibility pass.

### Go
- Нет P0/P1 visual regressions.
- Нет API contract drift на critical endpoints.
- Нет SLO regression сверх бюджета.

### No-Go
- Broken Light/Dark parity.
- Hardcoded accent outside token bridge.
- Unresolved a11y/semantic regressions.

---

## 10) Migration and rollback

### Migration
1. Token-first delivery.
2. Wave-by-wave release + parity snapshots.
3. Feature/API behind capability flags.
4. Mandatory real/fake + smart/fallback parity checks.

### Rollback
- Visual: disable motion/living gradient/accent profile flags, restore previous token pack.
- API: keep old endpoints in deprecation window, disable new behavior via server toggles.
- Incident flow: freeze wave -> revert patch -> rerun smoke + a11y + parity snapshots.

---

## 11) Missing data register and blockers

### Missing data
- Low-end GPU/CPU perf profiles (owner: FE Platform, ETA 2 weeks).
- User preference storage contract (owner: Backend Lead, ETA 1.5 weeks).
- Full semantic contrast matrix @200% (owner: QA Accessibility, ETA 1 week).
- SLO budget for realtime channels (owner: Platform/SRE, ETA 2 weeks).

### Current blockers for 100% readiness
1. Нет полной parity automation для touched flows.
2. Нет полного CI enforcement для no-hardcode accent.
3. Нет perf budget для low-end living background.
4. Не закрыт user theme/preferences contract.

### Blocker closure plan
- B1: parity snapshot suite (QA Platform, 1 week).
- B2: style lint + PR accent hardcode check (FE Platform, 4 days).
- B3: perf profiling + caps in token policy (UX Eng + FE Platform, 2 weeks).
- B4: `/user/theme-preferences` RFC + migration notes (Backend Lead, 1.5 weeks).

---

## 12) Prompt template for execution (copy/paste)

```text
Ты — Staff Frontend/UX архитектор и техлид по UI5 + backend contracts.
Используй документ `docs/ux/horizon-ios-roadmap-unified-plan.md` как единственный source of truth.

Задача: выполнить поэтапную доработку проекта по wave-плану (0..6 + A..D) и вернуть:
1) audit findings (top gaps),
2) implementation plan по неделям,
3) конкретные изменения по файлам/модулям,
4) acceptance criteria (измеримые DoD),
5) test strategy,
6) PR pack (несколько PR с evidence checklist).

Обязательные правила:
- twin-theme parity Morning+/Night+,
- token-first, без hardcoded accent в controls,
- API changes только с capability flags + contract tests,
- обязательные gates: parity/a11y/smoke/semantic contrast/CSS audit,
- у каждого risky change должен быть rollback.

Формат ответа: структурированный markdown с таблицами, owner/ETA, зависимостями и критическим путём.
```
