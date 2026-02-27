# UI5 Product Development Master Plan

## 1. Purpose and Scope
This document is the single source of truth for product and engineering development of the UI5 checklist system. It consolidates architecture evolution, UX quality, backend-integration contracts, operational excellence, testing governance, and rollout planning into one executable roadmap.

### Strategic goals
- Stabilize core user flows (search, smart filtering, detail card, edit/save, export).
- Raise UX consistency and accessibility to enterprise readiness.
- Enforce backend capability contracts and semantic version compatibility.
- Reduce incident risk through observability, SLOs, regression gates, and rollback playbooks.
- Improve delivery predictability with phase gates, ownership, and measurable KPIs.

---

## 2. Current-State Summary

### Product and UX
- Multi-stage search experience with Smart controls as the single source of truth; degraded metadata state is explicit and blocks misleading fallback search execution.
- Detail card supports read/edit lifecycle, lock orchestration, validation, and expanded row editing.
- UX governance assets exist but are fragmented across audit catalogs, taxonomies, and baselines.

### Technical architecture
- Layered use-case driven frontend architecture (controllers + service/usecase + util abstractions).
- Backend adapter supports fake/real backends and capability negotiation.
- Mock gateway includes OData semantics, metadata, filtering, batching, and analytics endpoints.

### Delivery posture
- Existing smoke gates and governance scripts are present.
- Documentation and plans are duplicated and distributed, increasing drift risk.

---

## 3. Target-State Vision

### Product outcomes
1. Search reliability: SmartFilter-first deterministic behavior, explicit degraded states, resilient empty-state messaging.
2. Detail reliability: stable load/hydration, lock-aware edit flow, robust save conflict and retry handling.
3. Decision support: trusted inline analytics and export with explicit source transparency.

### Engineering outcomes
1. Contract-first integration with explicit capability/version checks.
2. End-to-end automated quality gates across unit, smoke, UX baseline, API usage, and policy checks.
3. Operational readiness with observable SLOs, incident runbooks, and rollback drills.

---

## 4. Workstreams

## WS-A. Search and Smart Controls Hardening
### Objectives
- Eliminate SmartFilter inconsistency and state-sync drift.
- Ensure SmartTable-first reliability and remove contradictory UI states when metadata/capabilities degrade.

### Deliverables
- Unified filter extraction rules for id/lpc/status/date/equipment/observer.
- Deterministic rebind policy (only explicit triggers where required by product decision).
- Selection ID extraction normalization across all payload variants.
- Filter-hint messaging that accurately reflects active criteria.

### KPIs
- Search success rate ≥ 99.5% (no empty result due to client-side filter mismatch when backend has data).
- Detail open success from search selection ≥ 99.9%.

---

## WS-B. Detail Card Lifecycle Stabilization
### Objectives
- Keep card consistently renderable in all entry modes: read, create, copy, edit.
- Prevent data-loss and lock conflicts while preserving user intent.

### Deliverables
- Hardened lazy section loading for checks/barriers with skeleton strategy.
- Validation-first status transitions with clear missing-field guidance.
- Save conflict and own-lock-steal UX with deterministic outcomes.
- Toolbar and action state invariants (read/edit/dirty/lock ownership).

### KPIs
- Save flow completion rate ≥ 99%.
- Lock conflict resolution without manual refresh ≥ 95%.

---

## WS-C. UX Governance, Accessibility, and Visual Consistency
### Objectives
- Convert UX artifacts into enforceable acceptance criteria.
- Achieve predictable visual and interaction consistency across views.
- Execute a full style transition to SAP Horizon (Morning/Night) with iOS/macOS visual alignment.
- Build a branded UI shell while preserving existing glass backgrounds and key visual identity.

### Deliverables
- Canonical design governance checklist and message taxonomy integrated into CI gates.
- Baseline screenshot/state coverage maintained for key scenarios.
- Accessibility pass for focus order, labels, color contrast, and status communication.
- CSS refactor that resets/overrides default SAP UI5 standard object styling via controlled theme layer.
- Unified brand theme layer based on official SAP Horizon palette (Morning + Night) with iOS/macOS-inspired density, spacing and surface rhythm.
- Animated gradient background transitions for light/dark theme switching with performance-safe motion.
- Full branded shell (app frame, cards, toolbars, controls, typography hierarchy) across Search + Detail flows.

### KPIs
- 0 critical accessibility violations in release branch.
- 100% coverage of agreed key UX states in baseline set.
- 100% themed coverage of critical screens (no fallback to unstyled default UI5 controls in branded scope).
- Theme-switch animation frame stability >= 55 FPS on target baseline hardware.

---

## WS-D. Backend Capability Contract and Integration Resilience
### Objectives
- Keep UI and backend in sync under explicit compatibility rules.
- Degrade gracefully when capabilities are unavailable.

### Deliverables
- Capability schema and semver policy validation in runtime and CI.
- Backend contract drift gate with blocking thresholds.
- Metadata availability strategy for Smart controls (ready/pending/unavailable handling).
- Recovery pathways for partial backend failure (retry + warning state) without misleading fake/fallback search results.

### KPIs
- Contract drift incidents in production: 0.
- Metadata-related Smart control outages: < 1 per quarter.

---

## WS-E. Quality Engineering and Release Governance
### Objectives
- Make regressions difficult to ship.
- Standardize release confidence scoring.

### Deliverables
- Layered test matrix:
  - Static checks/lint syntax validation.
  - Unit and use-case smoke tests.
  - Browser smoke flows for search/detail critical paths.
  - UX and governance gates (taxonomy/a11y/api usage/state coverage).
- Wave-based regression gates and enterprise readiness thresholds.

### KPIs
- Escaped regression rate reduced by 50% within two release cycles.
- CI critical-gate pass rate ≥ 98% on mainline.

---

## WS-F. Observability, SLOs, and Incident Management
### Objectives
- Detect degradation before users report it.
- Shorten recovery time from incidents.

### Deliverables
- UX latency SLO definitions and reporting automation.
- Operational KPI instrumentation around search/detail/save/lock flows.
- SLO breach runbook + rollback playbook exercised in drills.

### KPIs
- MTTR under defined threshold for severity-2 incidents.
- SLO compliance > 99% for target interactions.

---

## 5. Phased Roadmap

### Phase 1 — Stabilize (Weeks 1–3)
- Fix high-frequency search/detail breakages.
- Normalize filter/state/id mappings.
- Close top-priority UI consistency defects.
- Establish minimum blocking CI gates.

### Phase 2 — Standardize (Weeks 4–7)
- Consolidate UX governance into executable checks.
- Expand browser smoke coverage for lifecycle edge cases.
- Harden backend contract validation and fallback behavior.

### Phase 3 — Scale (Weeks 8–12)
- Improve performance and resilience under load/latency.
- Raise gate strictness to enterprise thresholds.
- Institutionalize release quality scorecards.

### Phase 4 — Optimize (Continuous)
- Run quarterly architecture and UX audits.
- Retire deprecated flows and reduce complexity.
- Recalibrate KPIs/SLOs based on production telemetry.

---

## 6. Execution Model

### Ownership
- Product: scope, priorities, acceptance criteria.
- Frontend: user-flow correctness, UX consistency, UI performance.
- Backend/API: contract guarantees, metadata/odata behavior, capability declarations.
- QA/Release: gate quality, regression policy, test reliability.
- Operations: SLO monitoring, incident command, rollback readiness.

### Cadence
- Weekly triage of defects and contract drift.
- Bi-weekly roadmap checkpoint with KPI review.
- Release readiness review before each cut.

### Decision records
- Keep ADRs concise and updated for architectural decisions that affect contracts, caching, locking, and lifecycle orchestration.

---

## 7. Risk Register and Mitigation
- **Risk:** Smart control metadata instability.
  - **Mitigation:** explicit pending/unavailable states + fallback path + telemetry.
- **Risk:** Contract mismatch between UI and backend versions.
  - **Mitigation:** semver gate + runtime compatibility checks.
- **Risk:** UX drift due to rapid feature increments.
  - **Mitigation:** enforce visual/message/accessibility gates in CI.
- **Risk:** Hidden regressions in orchestration-heavy flows.
  - **Mitigation:** browser smoke suite across critical lifecycles.

---

## 8. Definition of Done (Program Level)
A release increment is considered done only if:
1. Critical user journeys pass automated smoke and manual sanity checks.
2. Contract and semver gates pass without exceptions.
3. UX governance, taxonomy, accessibility, and state-coverage gates pass.
4. SLO dashboard and incident playbooks are updated for new behavior.
5. Rollback strategy is validated for high-risk changes.

---

## 9. Immediate Next Actions
1. Complete view-level consistency pass (Search + Detail bindings, state transitions, empty states).
2. Close top 5 defects affecting search smart filters and detail navigation/edit lifecycle.
3. Enable strict CI profile for release branches.
4. Baseline KPI metrics and publish initial scorecard.
5. Schedule first quarterly architecture + UX audit.

---

## 10. Full Implementation Backlog (Decomposition to Subtasks)

This section decomposes each workstream into executable subtask packs so implementation can start immediately without ambiguity.

### WS-A. Search and Smart Controls Hardening — Subtasks
1. **Filter Contract Inventory**
   - Enumerate all filter payload shapes from SmartFilterBar, fallback state, backend query options.
   - Document canonical keys and aliases (`id`, `checklist_id`, `CHECKLIST_ID`, etc.).
2. **Normalization Layer Finalization**
   - Centralize filter value extraction (string, range, item arrays, key/value objects).
   - Add deterministic precedence for key aliases with first-non-empty semantics.
3. **Rebind Trigger Policy**
   - Define explicit trigger matrix: Search button, SmartFilter search event, status-chip press, clear action.
   - Prevent accidental implicit searches from unrelated state changes.
4. **Smart/Fallback Parity Matrix**
   - Verify parity for filtering, sorting, pagination, selection behavior, empty-state messaging.
   - Record accepted deltas (if any) with product sign-off.
5. **Selection/Navigation Hardening**
   - Normalize id extraction for all row shapes and event origins.
   - Add guardrails for missing ids and telemetry reason codes.
6. **Regression Coverage**
   - Add/update browser smoke flows for SmartFilter->Search->Open detail and fallback equivalents.

### WS-B. Detail Card Lifecycle Stabilization — Subtasks
1. **Entry Mode Contract**
   - Define invariant state model for READ/EDIT/CREATE/COPY transitions.
2. **Hydration and Skeleton Policy**
   - Formalize staged loading behavior for root/checks/barriers, including timeout + retry semantics.
3. **Validation Engine Hardening**
   - Map required fields by status transition and LPC conditions.
   - Make validation messages deterministic and localizable.
4. **Lock Lifecycle Matrix**
   - Cover acquire/heartbeat/status/release, own-lock conflict steal flow, killed-lock handling.
5. **Save/Error Taxonomy**
   - Standardize behavior for 409/412/network/timeouts/server errors with user-facing guidance.
6. **Detail Regression Suite**
   - Expand smoke tests for edit toggle, save conflict, unsaved close, expanded rows lifecycle.

### WS-C. UX Governance, Accessibility, Visual Consistency — Subtasks
1. **Design Rule Canonicalization**
   - Merge style, messaging, and interaction rules into enforceable checks.
2. **Visual Baseline Program**
   - Define key states + capture protocol + diff thresholds + approval workflow.
3. **A11y Conformance Sprint**
   - Evaluate labels, focus order, keyboard navigation, status color semantics.
4. **Message Taxonomy Enforcement**
   - Validate message IDs, severity mapping, and remediation guidance consistency.
5. **Governance Gate Integration**
   - Include all checks in CI with blocking policy by severity.

### WS-D. Backend Contract & Resilience — Subtasks
1. **Capability Schema Freeze**
   - Version and publish feature flags and compatibility envelope.
2. **Semver Policy Validation**
   - Enforce runtime and CI validation for min/max supported UI contract versions.
3. **Metadata Resilience**
   - Define behavior for metadata pending/unavailable/error with graceful fallback.
4. **Drift Detection**
   - Compare real backend capability payload against expected schema in CI gates.
5. **Failure Injection Scenarios**
   - Simulate partial backend outage, stale etags, lock contention, delayed batch responses.

### WS-E. QE and Release Governance — Subtasks
1. **Test Pyramid Re-baseline**
   - Map each critical journey to at least unit + smoke + gate coverage.
2. **Gate Severity Model**
   - Define block/warn/info thresholds and ownership for each gate.
3. **Release Scorecard**
   - Publish quality score per release candidate (defects, flaky rate, gate pass trends).
4. **Flake Management**
   - Add quarantining policy, rerun strategy, and mandatory remediation SLA.

### WS-F. Observability, SLO, Incident Response — Subtasks
1. **Telemetry Dictionary**
   - Define event names, fields, reason codes, and PII-safe policy.
2. **SLO Spec Finalization**
   - Set target, measurement window, error budget, and burn-rate alerts.
3. **Operational Dashboards**
   - Build search/detail/save/lock flow dashboards with drill-downs.
4. **Runbook Drills**
   - Execute quarterly tabletop and live rollback drills.

---

## 11. Required Steps for Full Plan Realization (End-to-End)

### Step 0 — Program Mobilization (Week 0)
- Appoint stream owners and backups.
- Approve scope, KPI baselines, and release cadence.
- Lock dependencies (backend, QA env, CI capacity).

### Step 1 — Discovery and Baseline (Week 1)
- Perform as-is mapping for flows, contracts, and test coverage.
- Capture current KPI baseline (search success, detail open success, save completion, gate pass rate).
- Produce prioritized defect and debt backlog.

### Step 2 — Stabilization Implementation (Weeks 1–3)
- Deliver WS-A and WS-B critical fixes.
- Introduce minimal blocking gates for syntax/smoke/contract checks.
- Validate no-regression on top user journeys.

### Step 3 — Standardization and Governance (Weeks 4–7)
- Roll out governance/a11y/message/visual checks.
- Enforce semver and capability drift checks.
- Normalize runbooks and error taxonomies.

### Step 4 — Scale and Hardening (Weeks 8–12)
- Increase performance and resilience test depth.
- Raise gate strictness from warning to blocking where mature.
- Publish release scorecards and trend analytics.

### Step 5 — Continuous Optimization (Post Week 12)
- Quarterly architecture + UX audits.
- SLO recalibration and telemetry schema evolution.
- Controlled simplification/deprecation program.

---

## 12. Plan QA (Self-Test): Errors and Missing Information Found

The plan itself was tested for implementability against criteria: **specificity, measurability, ownership clarity, dependency clarity, testability, rollout safety**.

### Errors / Weaknesses identified
1. **Missing explicit task decomposition** (fixed in Section 10).
2. **No step-by-step full implementation path** (fixed in Section 11).
3. **KPI definitions lacked formulas and data sources** (partially fixed in Section 13).
4. **No explicit dependency map** (fixed in Section 13).
5. **No blocking criteria per release gate severity** (partially fixed in Section 13).
6. **No clear evidence artifacts per milestone** (fixed in Section 13).
7. **No explicit "plan quality" validation checklist** (fixed below).

### Remaining information gaps (to be filled by owners)
- Exact telemetry backend/tooling source for KPI extraction.
- Production traffic thresholds for statistically valid KPI acceptance.
- Final list of mandatory accessibility criteria by policy/compliance framework.
- Environments matrix (dev/stage/preprod/prod parity requirements).
- Hard release cutoff policy when multiple medium-severity gates fail.

### Plan validation checklist (must pass before execution)
- [ ] Every subtask has owner and due date.
- [ ] Every KPI has formula + source + target + alert threshold.
- [ ] Every gate has severity and escalation owner.
- [ ] Every critical flow has automated coverage mapping.
- [ ] Rollback plan is tested for each high-risk stream.

---

## 13. Plan Enrichment (Operationally Actionable Additions)

### 13.1 KPI Formula Catalog
- **Search success rate** = successful search responses with non-error completion / all search attempts.
- **Detail open success** = successful detail hydrations / detail open intents.
- **Save completion rate** = successful saves / save attempts.
- **Gate pass rate** = passed blocking gates / total blocking gate executions.
- **SLO compliance** = requests within SLO target / all measured requests.

### 13.2 Dependency Map
- WS-A depends on WS-D metadata stability and WS-E smoke coverage.
- WS-B depends on WS-D lock/etag behavior and WS-F telemetry reason codes.
- WS-C depends on WS-E governance gate infrastructure.
- WS-E depends on stream owners delivering deterministic acceptance criteria.
- WS-F depends on event instrumentation availability across WS-A/WS-B.

### 13.3 Milestone Evidence Artifacts
Each phase exit must include:
- Changelog of delivered subtasks.
- Gate execution report with trend comparison.
- KPI snapshot and variance vs target.
- Open risk list with mitigation status.
- Rollback readiness statement.

### 13.4 Release Readiness Decision Rule
Release candidate is **GO** only when:
- No critical gate failures.
- KPI trends are non-degrading vs previous stable release.
- No unresolved P0/P1 defects in critical journeys.
- Rollback drill passed for high-risk deltas.

Otherwise status is **NO-GO** with explicit remediation plan and ETA.

---

## 14. Ultra-Granular Execution Map (One-Prompt-Per-Chapter Delivery)

Goal of this section: split each chapter into minimal implementation packets that can be fully executed in one prompt/session with clear inputs, actions, outputs, tests, and completion checks.

### 14.1 Delivery protocol for every chapter
For each chapter below, execute in exactly this sequence:
1. **Read scope**: identify files/modules affected.
2. **State assumptions**: list unknowns and default decisions.
3. **Implement**: apply code/config/doc changes.
4. **Validate**: run required checks listed in chapter.
5. **Report**: document result, risks, and follow-up deltas.

Completion invariant per chapter:
- [ ] Changes compile/parse.
- [ ] Required smoke/tests pass.
- [ ] Metrics/logging hooks (if required) are present.
- [ ] No unresolved TODO without owner/date.

---

### 14.2 WS-A chapters (Search & Smart Controls)

#### Chapter A1 — Filter Payload Census
**Inputs**: Search controller, smart filter use cases, backend query adapter.
**Steps**:
1. Enumerate all filter fields used in SmartFilterBar and fallback.
2. Enumerate all payload variants (`value`, `key`, `items`, `ranges`).
3. Build canonical alias table for each filter.
4. Add doc block near normalization function with alias table.
**Outputs**: Canonical mapping table committed.
**Validation**:
- `node --check controller/Search.controller.js`
- `node --check service/usecase/SearchSmartFilterFlowUseCase.js`

#### Chapter A2 — Unified Normalization Implementation
**Steps**:
1. Centralize extraction helpers in one module (or single source import).
2. Replace duplicated extraction logic call sites.
3. Enforce first-non-empty resolution order.
4. Add defensive handling for null/empty/range-only cases.
**Outputs**: deterministic extracted state for id/lpc/status/date/equipment/observer.
**Validation**:
- syntax checks for changed JS files
- targeted smoke for SmartFilter change and Search click

#### Chapter A3 — Trigger Matrix Enforcement
**Steps**:
1. List all trigger origins (button/search/filter change/clear/status chips).
2. Enforce allowed triggers in policy use case.
3. Add telemetry reason codes for skipped triggers.
4. Verify no auto-search side effects from passive state changes.
**Validation**:
- search trigger smoke scripts
- manual check: filter change alone does not run forbidden request path

#### Chapter A4 — Smart/Fallback Parity Closure
**Steps**:
1. Compare parity matrix: filter semantics, result summary, selection, empty state.
2. Resolve mismatches or document accepted deviations.
3. Align ID display and selection extraction semantics.
**Validation**:
- fallback search smoke
- smart search smoke
- parity checklist marked complete

#### Chapter A5 — Selection and Navigation Hardening
**Steps**:
1. Normalize id extraction for row object permutations.
2. Add missing-id guard messaging.
3. Ensure detail open intent always receives resolved id.
**Validation**:
- selection lifecycle smoke (smart/fallback)
- open detail guard smoke

---

### 14.3 WS-B chapters (Detail lifecycle)

#### Chapter B1 — Entry-State Contract
**Steps**:
1. Define state table for READ/EDIT/CREATE/COPY with allowed transitions.
2. Encode invariants in orchestrators/use cases.
3. Prevent illegal state flips via guard returns.
**Validation**:
- route match lifecycle smoke
- create/copy/open existing manual checks

#### Chapter B2 — Skeleton + Lazy Load Reliability
**Steps**:
1. Ensure skeleton appears before async rows load.
2. Add timeout/fallback behavior and visible user message.
3. Ensure independent completion of checks/barriers loaders.
**Validation**:
- detail skeleton smoke
- no permanent busy state after partial failure

#### Chapter B3 — Validation & Status Transition Rules
**Steps**:
1. Map required fields by target status/LPC.
2. Show deterministic field-level and summary errors.
3. Block save/status transition when invalid.
**Validation**:
- toolbar validation smoke
- status command smoke

#### Chapter B4 — Lock Conflict and Recovery
**Steps**:
1. Validate acquire/heartbeat/status/release transitions.
2. Implement own-lock steal prompt and outcomes.
3. Handle killed lock and downgrade mode safely.
**Validation**:
- lock lifecycle smoke
- save during lock conflict smoke

#### Chapter B5 — Save Error Taxonomy
**Steps**:
1. Map backend error types to consistent UI outcomes.
2. Standardize retry/force-update rules.
3. Ensure telemetry tags each error class.
**Validation**:
- save conflict flow smoke
- save error outcome smoke

---

### 14.4 WS-C chapters (UX, a11y, consistency)

#### Chapter C1 — Visual Rule Enforcement
**Steps**:
1. Convert style/interaction rules into checkable assertions.
2. Add failing examples and remediation notes.
3. Wire assertions into CI gate scripts.
**Validation**:
- governance gate script pass

#### Chapter C2 — Baseline State Coverage
**Steps**:
1. Define mandatory screenshot states for Search/Detail.
2. Capture baseline artifacts.
3. Add diff threshold policy and approval path.
**Validation**:
- screenshot generation script
- state coverage gate pass

#### Chapter C3 — Accessibility Closure
**Steps**:
1. Audit labels/roles/focus order/keyboard actions.
2. Patch violations by severity.
3. Re-run a11y gate and document residual non-blockers.
**Validation**:
- `node scripts/a11y-gate.js`

#### Chapter C4 — Message Taxonomy Consistency
**Steps**:
1. Ensure each user-facing event maps to standard message type and tone.
2. Remove duplicate or contradictory messages.
3. Gate taxonomy drift in CI.
**Validation**:
- `node scripts/message-taxonomy-gate.js`


#### Chapter C5 — SAP Horizon + iOS/macOS Style Migration
**Steps**:
1. Build token map from official SAP Horizon Morning/Night palette to project CSS variables.
2. Define iOS/macOS alignment rules (corner radius, spacing cadence, toolbar density, elevation behavior) without breaking SAP UI5 semantics.
3. Refactor existing CSS into layered architecture: `base reset -> horizon tokens -> brand shell -> component overrides`.
4. Remove accidental one-off styles and replace with reusable utility classes/tokens.
5. Ensure all critical controls inherit branded layer while preserving accessibility contrast targets.
**Validation**:
- horizon theme token audit report
- critical-screen visual diff report (morning/night)

#### Chapter C6 — Branded Shell, Glass Surfaces, and Animated Gradient Themes
**Steps**:
1. Preserve existing glass backgrounds and merge them into new branded shell visual language.
2. Implement app-shell level background gradients for light/dark themes using official Horizon-compatible color ranges.
3. Add subtle theme-transition animations (background/overlay) with reduced-motion fallback.
4. Tune motion duration/easing to avoid jank and maintain responsiveness.
5. Add regression snapshots for theme switch states (before/during/after transition).
**Validation**:
- screenshot generation script for morning/night + transition states
- performance smoke check for theme switch animation
- reduced-motion accessibility check

---

### 14.5 WS-D chapters (Backend contract resilience)

#### Chapter D1 — Capability Contract Lockdown
**Steps**:
1. Freeze capability schema fields and defaults.
2. Validate parser compatibility for missing/new fields.
3. Version contract and update compatibility table.
**Validation**:
- capability fixture checks
- backend contract drift gate

#### Chapter D2 — Semver Policy Enforcement
**Steps**:
1. Validate min/max version ranges on startup.
2. Surface explicit incompatible-version reason.
3. Add CI tests for boundary versions.
**Validation**:
- semver policy gate pass

#### Chapter D3 — Metadata Failure Modes
**Steps**:
1. Handle pending/unavailable/error separately.
2. Ensure fallback UI remains searchable.
3. Add telemetry for metadata outage reasons.
**Validation**:
- metadata recovery smoke
- smartfilter fallback transition smoke

#### Chapter D4 — Failure Injection Matrix
**Steps**:
1. Simulate stale etag, partial batch failure, lock contention.
2. Verify deterministic UI outcomes and no stuck state.
3. Add regression checks for each scenario.
**Validation**:
- conflict + retry smoke scripts
- batch interaction smoke

---

### 14.6 WS-E chapters (QE + release)

#### Chapter E1 — Coverage Matrix Build
**Steps**:
1. Map each critical journey to exact test scripts.
2. Mark uncovered journeys and create backlog tickets.
3. Add gate that fails on uncovered criticals.
**Validation**:
- CI smoke gate + matrix report

#### Chapter E2 — Gate Severity Policy
**Steps**:
1. Tag each gate as block/warn/info.
2. Add escalation owner and SLA per gate.
3. Enforce block-only release policy in release script.
**Validation**:
- enterprise readiness gate pass

#### Chapter E3 — Flake Control
**Steps**:
1. Track flaky tests with frequency threshold.
2. Quarantine with expiry and owner.
3. Auto-fail if quarantine TTL exceeded.
**Validation**:
- flake report generation

#### Chapter E4 — Release Scorecard Automation
**Steps**:
1. Aggregate defect, gate, performance, flake metrics.
2. Emit release scorecard artifact.
3. Require scorecard in release checklist.
**Validation**:
- scorecard artifact present in CI output

---

### 14.7 WS-F chapters (Observability + SLO)

#### Chapter F1 — Telemetry Contract
**Steps**:
1. Define event schema for search/detail/save/lock.
2. Ensure reason codes are exhaustive and non-overlapping.
3. Validate no PII leakage.
**Validation**:
- telemetry schema lint/check script

#### Chapter F2 — SLO Measurement Pipeline
**Steps**:
1. Define query logic and windows for each SLO.
2. Implement burn-rate alerts.
3. Validate alert routing.
**Validation**:
- synthetic breach test

#### Chapter F3 — Dashboard and Runbooks
**Steps**:
1. Build dashboard panels mapped to KPIs.
2. Link each panel to troubleshooting steps.
3. Run drill and capture postmortem improvements.
**Validation**:
- drill report artifact

---

### 14.8 “One Prompt to Complete One Chapter” template
Use this exact prompt skeleton when executing any chapter:
1. **Goal**: “Implement Chapter <ID> exactly as defined in `DEVELOPMENT_PLAN.md`.”
2. **Scope**: list files/modules allowed.
3. **Constraints**: no behavior outside chapter; preserve API compatibility.
4. **Tasks**: copy chapter micro-steps 1..N.
5. **Validation**: copy chapter command list.
6. **Done criteria**: checkboxes from 14.1.

---

### 14.9 Full-plan completion order (strict)
1. A1 → A2 → A3 → A4 → A5
2. B1 → B2 → B3 → B4 → B5
3. C1 → C2 → C3 → C4 → C5 → C6
4. D1 → D2 → D3 → D4
5. E1 → E2 → E3 → E4
6. F1 → F2 → F3

Exit criteria for entire plan:
- [ ] All chapter validations pass.
- [ ] KPI targets reached for two consecutive release cycles.
- [ ] No open P0/P1 defects in covered journeys.
- [ ] GO/NO-GO rule indicates GO.

---

## 15. Strict Delivery Response Plan Addendum (Execution Kickoff for Branded UI)

This addendum operationalizes the branded UI transition and aligns with the requested strict 2–4 week cycle.

### 15.1 Top audit findings to close first
1. Missing full parity automation for touched flows (Morning/Night + dual mode).
2. Missing CI enforcement for hardcoded accent usage.
3. No low-end performance budget for living background.
4. Missing finalized user theme/preferences API contract.
5. Missing one unified mandatory 5-gate CI package.
6. Smart/fallback parity not yet strict release blocker.
7. API evolution risk without capability negotiation enforcement.
8. Reset governance may break UI5 semantics without scoped policy.
9. Semantic contrast matrix at 200% not fully covered.
10. Rollback discipline not mandatory per risky PR.

### 15.2 4-week strict execution sequence
- **Week 1 (foundation):** CSS/token audit, hardcoded-accent blocking gate, parity checklist draft, capability schema freeze start.
- **Week 2 (parity/API):** paired snapshots + dual-mode smoke in CI, semantic contrast automation, golden contract test coverage.
- **Week 3 (perf/preferences):** low-end profiling + animation caps + reduced-motion validation, `/user/theme-preferences` RFC + endpoint scaffold behind capability flags.
- **Week 4 (hardening):** controls/motion verification, Go/No-Go rehearsal, release governance + rollback drill.

### 15.3 Mandatory 5 CI gates (release-blocking)
1. Parity snapshots gate (Morning/Night).
2. Accessibility gate.
3. Smoke gate (smart + fallback, real/fake mode).
4. Semantic/contrast gate.
5. CSS governance gate (token-first, no hardcoded accent).

### 15.4 PR pack for full cycle
1. CI gate hardening.
2. Style governance and token enforcement.
3. Theme parity automation.
4. API contract stabilization (capabilities + golden tests).
5. Theme preferences contract (`/user/theme-preferences`) with migration notes.

### 15.5 Kickoff status (started)
- [x] Plan updated with Horizon + iOS/macOS brand migration chapters (C5/C6).
- [x] CSS governance gate script scaffolded: `scripts/css-accent-governance-gate.js`.
- [x] Enterprise UX gate chain updated to include CSS governance gate.
- [x] Pair snapshots baseline matrix wired into CI.
- [x] Theme preferences contract RFC + backend capability flag.

---

## 16. WS-C Refinements: Full Style-Surface Inventory and Strict Migration Clarifications

This section incorporates the latest implementation clarifications and defines exactly what must be reset/polished for complete branded migration.

### 16.1 UI5 baseline and scope lock
- UI5 baseline: **1.71.28**.
- Scope includes all APIs and implicit runtime surfaces used in app shell libraries:
  - `sap.m`, `sap.f`, `sap.ui.table`, `sap.ui.comp`, `sap.ui.core`.
- Migration rule: no style reset may break semantic/behavioral contracts of UI5 controls.

### 16.2 Controls and runtime surfaces requiring reset/polish (authoritative inventory)
The following inventory is in-scope for reset/improvement and must be tracked in parity reports:

1. **Core app shell/layout surfaces**
   - `sap.m.Page`, `sap.m.NavContainer`, `sap.f.FlexibleColumnLayout`, app containers, shell backgrounds.
2. **Primary interaction controls (`sap.m`)**
   - `Button`, `Input`, `SearchField`, `Select`, `ComboBox`, `MultiComboBox`, `DatePicker`, `TimePicker`, `Switch`, `CheckBox`, `SegmentedButton`, `MenuButton`.
3. **Data display controls (`sap.m`)**
   - `Table`, `List`, `ColumnListItem`, `ObjectStatus`, `ObjectIdentifier`, `MessageStrip`, `Panel`, `Title`, `Text`.
4. **Dialog and overlay surfaces (`sap.m`)**
   - `Dialog`, `Popover`, `ResponsivePopover`, `ActionSheet`, `BusyDialog`, `SelectDialog`, `ViewSettingsDialog`.
5. **Smart controls (`sap.ui.comp`)**
   - `SmartTable`, `SmartFilterBar`, `FilterBar`, `ValueHelpDialog`, `P13nDialog`.
6. **Advanced tables (`sap.ui.table`)**
   - `Table`, `TreeTable`, `AnalyticalTable`, `Column`.
7. **Grid/card shell surfaces (`sap.f`)**
   - `GridList`, `Card`, `DynamicPage`, `ShellBar`.
8. **Implicit runtime modules (must be style/a11y covered)**
   - `sap/m/MessageBox`, `sap/m/MessageToast`, `sap/ui/core/Fragment`, etc.

### 16.3 Clarified polish policy (must enforce)
- **Reset layer:** Wave 61 + Wave 62 CSS reset/skin overlays.
- **Token policy:** radius, glass-bg, glass-border, focus-ring, motion, opacity are mandatory tokenized roles.
- **Async safety:** pending guard, busy state, model-first reconciliation.
- **Accessibility:** focus-visible, reduced-motion, contrast-safe states.
- **No hardcoded accent:** enforced by CSS governance gate and CI blocking.

### 16.4 Non-negotiable acceptance add-ons for WS-C
- 100% mapping of in-scope controls to token roles.
- 100% Morning/Night parity for touched flows.
- Smart/fallback parity gates treated as release-blocking.
- Zero uncontrolled global reset overrides that break UI5 semantics.
- Required rollback section in every risky style PR.

---

## 17. Master Execution Prompt (for Full End-to-End Style Migration)

Use this prompt to execute complete branded migration by plan, chapter-by-chapter, with strict quality gates.

### 17.1 Ready-to-run prompt

```text
You are implementing the full branded style migration for this UI5 application according to docs/DEVELOPMENT_PLAN.md.

MANDATORY OBJECTIVE
- Complete WS-C including C1..C6 and strict addendum (Section 15 + 16).
- Deliver SAP Horizon Morning/Night + iOS/macOS-aligned branded shell.
- Preserve existing glass visual identity while refactoring CSS into controlled layered architecture.

SCOPE AND BASELINE
- UI5 baseline is 1.71.28.
- In-scope controls/runtime surfaces are those listed in DEVELOPMENT_PLAN Section 16.2.
- Do not break UI5 semantics, Smart controls behavior, accessibility, or fallback mode behavior.

IMPLEMENTATION PHASES (execute sequentially)
1) Foundation & governance:
   - Build/verify token map and CSS layer architecture (`base reset -> horizon tokens -> brand shell -> component overrides`).
   - Enforce no-hardcoded-accent policy with CI gate.
2) Control-surface migration:
   - Style pass on shell/layout, primary controls, table/list surfaces, dialogs/overlays, smart controls, ui.table surfaces.
   - Record token-role mapping coverage for each control family.
3) Theme parity & motion:
   - Implement Morning/Night parity snapshots for touched flows.
   - Add light/dark gradient transitions + reduced-motion fallback.
   - Validate animation performance budget on low-end profile policy.
4) Accessibility and semantic safety:
   - Ensure focus-visible, keyboard paths, contrast-safe semantic states, message consistency.
5) Contract and release hardening:
   - Ensure mandatory gates are release-blocking: parity/a11y/smoke/semantic/CSS.
   - Ensure rollback instructions exist for risky changes.

REQUIRED CI/GATE VALIDATION
- node scripts/css-accent-governance-gate.js
- node scripts/a11y-gate.js
- node scripts/message-taxonomy-gate.js
- node scripts/ux-state-coverage-gate.js
- node scripts/theme-parity-gate.js
- node scripts/control-token-mapping-gate.js
- node scripts/unit-smoke.js
- node scripts/ci-smoke-gate.js
- bash scripts/ci/enterprise-ux-gate.sh

DELIVERABLES
- Updated CSS with token-first layered architecture.
- Updated/added gates and scripts required by WS-C and Section 15/16.
- Snapshot evidence and parity report for Morning/Night.
- Control-to-token mapping report for all in-scope controls (artifact: docs/artifacts/control-token-mapping.json).
- Changelog of completed chapters and unresolved risks.

DONE CRITERIA (all must pass)
- No hardcoded accent violations.
- 0 critical accessibility violations.
- 100% touched-flow Morning/Night parity.
- Smart/fallback parity remains stable.
- Required gates pass (or documented environment limitation only).
- Rollback plan attached for risky deltas.
```

### 17.2 Operator notes
- Execute work in PR packs from Section 15.4.
- If a gate cannot run due to environment limitations, explicitly log it as warning and keep all available gates green.
- Do not ship partial migration without parity and accessibility evidence artifacts.


---

## 18. Full Migration Closure Criteria and Certificate

The migration theme is considered fully complete only when:
- `scripts/finalize-style-migration.js` exits with code 0.
- `docs/artifacts/final-style-migration-certificate.json` is generated with `migrationStatus = completed`.
- All mandatory gates and smoke checks are listed as pass in certificate results.
- Rollback playbook artifact remains present and review-ready.

Closure command:
- `node scripts/finalize-style-migration.js`

Closure artifact:
- `docs/artifacts/final-style-migration-certificate.json`

---

## 19. Remediation Update (2026-02)

### 19.1 Completed fixes in current hardening cycle
- Smart-search request stability: overlapping smart-table rebinds are now guarded by in-flight protection in Search flow.
- Theme baseline switched to Horizon pair (`sap_horizon` / `sap_horizon_dark`) and bootstrap default aligned.
- Theme lifecycle stabilization: switching marker cleanup is synchronized with UI5 `themeChanged` callback.
- Smart-control availability logic corrected for metadata failure path (controls are now explicitly disabled when metadata is unavailable/failed).
- Search trigger orchestration hardened in controller to handle both sync and async trigger outcomes safely.
- CSS parsing hygiene improved: malformed section header lines in module stylesheets were normalized to valid CSS comments.

### 19.2 Additional defects found and fixed during review
- Incorrect SmartControl availability branch (`enabled=true` on metadata failure) caused unit smoke failure and invalid runtime behavior.
- Promise-chain hazard in search trigger usage (controller expected Promise while smart path returned sync result).
- Non-CSS comment text outside comment blocks in stylesheet modules, which could cause parser recovery side effects in some engines.

### 19.3 Updated short-term roadmap (next 2 sprints)
1. **Search observability uplift**
   - Add explicit client telemetry counters for `rebind_started`, `rebind_skipped`, `rebind_completed`, `rebind_timeout`.
   - Define alert threshold for rebind storms (e.g., >20 rebind attempts / 10s / session).
2. **Theme reliability uplift**
   - Add automated browser smoke that toggles theme rapidly (10+ toggles) and asserts final class/theme consistency.
   - Add unit checks for localStorage theme fallback migration from legacy values.
3. **CSS governance uplift**
   - Add linter gate to reject non-comment decorative section lines and malformed block headers.
   - Add duplicate-selector reporting for `css/modules/*.css` as non-blocking warning, then tighten to blocking.
4. **Smart/Fallback parity uplift**
   - Add explicit parity tests for filter composition (`id/lpc/status/date/equipment/observer`) across smart and fallback paths.
   - Add test for metadata-failed startup ensuring fallback table remains operational and user-visible.

### 19.4 Exit criteria for this remediation wave
- `node scripts/unit-smoke.js` green.
- `node scripts/ci-smoke-gate.js` green.
- `node scripts/a11y-gate.js` green.
- `node scripts/theme-parity-gate.js` green.
- No known P1/P2 defects open for search trigger, smart-control availability, or theme switching.


### 19.5 Responsive toolbar overflow improvements
- Migrated key detail-card action rails from `Toolbar`/`HBox` to `OverflowToolbar` where actions can be clipped on resize.
- Added explicit `OverflowToolbarLayoutData` priorities for save/add/expand actions, so low-priority actions collapse into the overflow menu (`...`) instead of disappearing or being cut off.
- Added follow-up task to apply the same pattern to remaining dialogs/fragments with dense action rails.


### 19.6 UI/CSS/Theme consolidation strategy (local + custom themes)
- Keep **single CSS entrypoint** (`css/style.css`) as contract source, but enforce deduplication and parser-safe comments.
- Maintain Horizon as default runtime pair (`sap_horizon` / `sap_horizon_dark`) and treat additional theme variants as opt-in.
- Add a dedicated theme-loading compatibility check before enabling local `themes/` artifacts in production (verify runtime path compatibility with UI5 theme root conventions).
- Introduce performance budget for styling:
  - no duplicate selector blocks in style bundle,
  - no repeated heavy backdrop/blur declarations without measurable value,
  - preserve reduced-motion behavior and accessibility gates.
- Phase rollout: (1) audit + dedupe, (2) parity tests, (3) optional local-theme activation behind capability flag.


### 19.7 Animated theme-transition and background gradient tuning
- Added a corner-origin theme transition overlay animation ("flood" effect) tied to `theme-switching` state for smoother perceived theme change.
- Strengthened animated background gradient visibility and slowed drift speed for a calmer native-like motion profile.
- Updated visual palette targets:
  - Light mode: brighter white/blue/soft-gray gradient composition.
  - Dark mode: richer violet/near-black/graphite gradient composition.
- Kept reduced-motion fallback strict (`prefers-reduced-motion`) so accessibility behavior remains compliant.


### 19.8 Duplicate selector governance rollout
- Added `scripts/css-duplicate-selector-gate.js` as **non-blocking warning gate** in current phase.
- Integrated warning gate into enterprise UX chain with `|| true` to collect signal without blocking releases.
- Next phase action: remove known duplicates in `style-core.css`, then switch this gate to blocking mode.


### 19.9 Card fusion + iOS switch visual polish
- Implemented `mergeBlock` utilities to visually fuse adjacent content cards into a single rounded rectangle when blocks are in direct contact.
- Increased glass depth (blur + transparency + saturation) for key content surfaces while preserving readability.
- Added modern iOS-like switch styling (track/knob/shadow/on-state gradient) for a more native control feel.
- Hardened test-login dialog corner rendering by applying unified glass clipping and section radius normalization.


### 19.10 Theme-specific accent buttons, motion context, and contour fusion
- Added theme-specific fully-filled accent button visual language for light and dark theme variants.
- Added contextual interaction motion for action buttons (hover lift + press feedback) with restrained timing.
- Added smoother Flexible Column Layout transitions (width/flex-basis/transform) for column open/close states.
- Added bottom cone glow treatment for card surfaces and fusion logic for connected blocks (top/middle/bottom) to preserve single-contour perception.
- Aligned test-login dialog footer surface and clipping to remove lower seam artifacts.


### 19.11 Search shell recovery + metadata fallback UX
- Moved request/load error strip above filter card to preserve user context and make degraded-state reason visible before interaction.
- Enabled fallback quick-filters/search/fallback-table visibility when Smart controls are unavailable (metadata error path).
- Removed static top-left brand label in app shell and finalized fully circular theme toggle button.
- Unified search-shell background fill to remove footer-like seam and tuned card bottom-cone intensity for cleaner contour.

### 19.12 Stability hardening checklist (locks / modes / autosave / cache boundaries)
- **Workflow lock lifecycle**
  - Enforce explicit lock finite-state machine (`READ_UNLOCKED -> EDIT_LOCK_PENDING -> EDIT_LOCKED -> READ_UNLOCKED`) with transition guards.
  - Add stale-lock preemption strategy: when `lock_expires` is reached or heartbeat reports `is_killed`, force-switch to READ, disable edit switches/buttons, and present a single conflict banner.
  - Add lock reacquire backoff (short retry budget) for intermittent network loss before full downgrade to READ.
- **UI modes and unsaved changes safety**
  - Keep centralized unsaved-change guard for route transitions and browser unload.
  - On lock interception, run `trySave` only when delta exists; otherwise skip network write and mark decision as `NO_CHANGES`.
  - Normalize mode-dependent switch behavior: all edit-only switches must be disabled when `isLocked=false` or `lockOperationPending=true`.
- **Autosave and CRUD payload correctness**
  - Continue delta-only autosave payloads with explicit operation semantics (`create|update|delete`) for rows in checks/barriers.
  - Preserve immutable backend fields filtering (`changed_on`, `server_changed_on`, `version_number`, `_cacheTimestamp`) before transport.
  - Add telemetry counters for autosave outcomes (`saved`, `skipped_no_delta`, `conflict`, `network_error`).
- **Smart cache boundaries (critical policy)**
  - Cache only checklist dataset snapshots (`checkLists`) in SmartCache.
  - Do **not** cache UI layout state, field composition, or per-view rendering settings.
  - Allowed UI persistence remains only user theme preference (`sap_ui5_theme`) in localStorage.
  - Add guardrails to reject non-allowlisted cache keys and emit a `cacheSkipped:not_allowed` event for observability.
- **Operational drills**
  - Add weekly smoke scenario: lock expires during edit + pending autosave + route change.
  - Add release gate scenario: backend unavailable during autosave while lock status probe is degraded.


## 11. 2026-Q1 Consistency and Performance Addendum

### Model/View state invariants (must hold)
- `state>/layout` controls FCL semantic state (OneColumn/TwoColumns*), while `state>/columnSplitPercent` controls optional visual split ratio only for wide screens.
- Smart search execution is allowed only when `view>/useSmartControls === true`; degraded metadata state must show warning messaging and avoid implicit fallback execution.
- Empty-state text is derived from `state>/loadError` + data presence and is applied to the currently active table binding target.
- Runtime config may tune split percent through `layout.columnSplitPercent` or `variables.fclBeginColumnPercent` and must be clamped to [20..80].

### UX/UI hardening checklist
- No toolbar overlaps on <=760px: controls wrap or collapse to icon-only where required.
- No table-header text crushing: horizontal overflow container with minimum semantic width is enforced.
- Rounded contour and switch visibility standards apply consistently in ON/OFF and dark/light modes.
- FCL custom split is disabled automatically on narrow viewports to prevent column collision and content clipping.

### Performance checklist
- Avoid excessive layout thrash during resize by scheduling FCL split application through `requestAnimationFrame`.
- Avoid redundant DOM writes by caching the last applied split signature.
- Detach model bindings and global listeners in controller `onExit` to prevent memory leaks in long sessions.

### Governance updates
- Mandatory pre-release checks: `unit-smoke`, `ci-smoke-gate`, and browser smoke flows where Playwright runtime is available.
- Any UX/CSS changes that affect responsive behavior must include a mobile-width screenshot artifact in CI-capable environments.
