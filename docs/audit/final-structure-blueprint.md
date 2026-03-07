# Final Structure Blueprint (Gateway-Only)

## Runtime Layers
- `controller/`: orchestration only (no business logic, no direct backend work).
- `controller/support/`: UI orchestration helpers and view bindings.
- `service/domain/**/usecases/`: canonical business behavior.
- `service/domain/*Facade.js`: feature entry points for controllers.
- `service/framework/`: effect pipeline, component runtime wiring, context/runtime helpers.
- `infra/`: adapters and low-level integration with UI5/OData infrastructure.
- `manager/`: runtime managers; single import entry is `manager/ManagerFacade.js`.
- `model/` and `util/`: state schema/defaults and pure utilities.

## Canonical Import Entrypoints
- Managers: `checklist/app/manager/ManagerFacade`
- Search domain: `checklist/app/service/domain/search/SearchFacade`
- Detail domain: `checklist/app/service/domain/detail/DetailFacade`
- Export orchestration: `checklist/app/service/domain/search/ExportFacade`
- QA pipeline validators: `scripts/lib/qa-pipeline-config.js`
- Component init feedback support: `checklist/app/service/framework/ComponentInitFeedbackSupport`
- Shared feedback/banner contract:
  - `checklist/app/service/framework/FeedbackBannerState`
  - `checklist/app/service/framework/RuntimeInput`
- App controller action modules:
  - `checklist/app/controller/support/AppControllerLifecycleActions`
  - `checklist/app/controller/support/AppControllerOverlayActions`
  - `checklist/app/controller/support/AppControllerShellActions`
  - `checklist/app/controller/support/AppControllerStateActions`
  - `checklist/app/controller/support/AppControllerDomActions`
- Detail validation summary support: `checklist/app/controller/support/DetailValidationSummarySupport`
- Detail controller action modules:
  - `checklist/app/controller/support/DetailChecklistCoreSupport`
  - `checklist/app/controller/support/DetailChecklistStateActions`
  - `checklist/app/controller/support/DetailChecklistRowActions`
  - `checklist/app/controller/support/DetailAttachmentLocationActions`

## Script Segmentation
- Active QA/runtime contract scripts stay under `scripts/` root and `scripts/ci/`, `scripts/gates/`, `scripts/lib/`.
- Unified facade-contract smoke is tracked via:
  - `scripts/browser-smoke-domain-facade-contract.py`

## Gateway-Only Invariants
- Runtime path must not use `fetch`, `XMLHttpRequest`, axios, or REST fallbacks.
- Runtime path must not use local JSON dataset fallback patterns.
- Runtime settings are sourced from SAP Gateway (`RuntimeSettingsSet` via OData model), without fallback branch.

## Dedup Rules (Active)
- One implementation per behavior domain (effects, row-ops, attachment DnD/upload policy, export flow).
- Controllers delegate to support/actions and facades; domain behavior lives in usecases.
- Shared policy/state logic must be centralized and reused, not copied per feature.

## Current Follow-Up Queue
- Keep `service/framework/ComponentInitRuntime.js` as orchestrator only; continue moving any new logic to bounded supports.
- Split `controller/support/DetailControllerActions.js` into smaller domain action modules.
