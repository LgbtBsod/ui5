# Namespace, Module, and UI Binding Audit (UI5 1.71)

## PHASE 1 — Full project inventory

Static inventory (generated from repository scan):
- JavaScript modules: **157**
- XML views: **3**
- XML fragments: **9**
- Core descriptors present: `manifest.json`, `Component.js`, `index.html`

Top module groups:
- Controllers: 5
- Services (incl. use cases): 95
- Managers: 10
- Utils: 17
- Models: 2
- Controls: 1

Artifact with full machine-readable module inventory:
- `docs/artifacts/static-audit.json`

## PHASE 2 — Namespace map and dependency graph checks

### Checks executed
- `sap.ui.define` dependencies resolved against local filesystem for `sap_ui5/*` namespace.
- Dependency array vs factory parameter count consistency.
- Routing target/view existence cross-check.

### Results
- Broken `sap_ui5/*` import paths: **0**
- Dependency/parameter mismatches: **0**
- Missing routing targets: **0**
- Missing target views: **0**

### Namespace consistency observations
- Module namespace mapping is consistent with file paths for active app modules.
- No incorrect casing issues detected in imports for UI runtime modules.

## PHASE 3 — XML view/fragment binding audit

### Extracted and validated
- Event bindings from views/fragments (`press`, `change`, `search`, `beforeRebindTable`, `initialise`, etc.).
- Controller method existence for event-handler bindings.
- Formatter attribute occurrences.

### Results
- XML event bindings discovered: **46**
- XML → controller missing method references: **0**
- Fragment/controller context mismatches: none detected by static checks.

## PHASE 4 — Routing validation

Validated manifest routing graph:
- Routes → targets: valid.
- Targets → views: valid.
- Views referenced by routing exist and use matching controller namespaces.

No routing linkage defects found.

## PHASE 5 — Naming consistency audit

### Conventions assessed
- Controllers: `*.controller.js` ✅
- Managers: `*Manager.js` ✅ (active set)
- Event handlers predominantly `on*` ✅
- Private helpers predominantly `_camelCase` ✅
- Constants mostly `UPPER_SNAKE_CASE` ✅

### Noted deviations (non-breaking, informational)
- Use case filenames follow current project style `*UseCase.js` (not `.uc.js`).
  - This is internally consistent across project and was **not** changed to avoid broad non-functional churn.

## PHASE 6 — Duplicate logic detection

### Reviewed duplicate-prone domains
- SmartTable rebinding flow and filter merge (search)
- OData access wrappers and error mapping (gateway client + backend adapter)
- Timer handling and busy-state orchestration (manager/use-case layers)

### Findings
- Duplication is largely reduced by existing orchestration use cases.
- Some intentional overlap remains between lifecycle/presentation use cases (acceptable separation of concerns).
- No high-confidence, zero-risk large consolidation was applied in this pass to preserve runtime behavior.

---

## Issue list identified before refactor

1. `BackendAdapter.js` contained dead code from old mode-switching design:
   - `_readUrlMode()` no longer used.
   - local variable `sMode` in `_selectBackend()` unused.

2. Potential dead-module candidates from static graph (manual proof required before delete; **not deleted in this pass**):
   - `manager/interval/BaseIntervalManager.js`
   - `service/autosave/DeltaBuilder.js`
   - `service/backend/ODataGatewayAdapter.js`
   - `service/usecase/KpiSnapshotExportUseCase.js`

(Excluded false positives tied to non-JS references: `control/ThemeToggle.js` is used by XML; `util/FlexibleRouter.js` is loaded via `sap.ui.requireSync`.)

---

## Safe refactor performed after analysis

- Removed unused `_readUrlMode()` function and unused local `sMode` variable from `service/backend/BackendAdapter.js`.
- No behavior change intended; backend remains forced to real mode.

