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
- Managers: `sap_ui5/manager/ManagerFacade`
- Search domain: `sap_ui5/service/domain/search/SearchFacade`
- Detail domain: `sap_ui5/service/domain/detail/DetailFacade`
- Export orchestration: `sap_ui5/service/domain/search/ExportFacade`
- QA pipeline validators: `scripts/lib/qa-pipeline-config.js`
- Component init feedback support: `sap_ui5/service/framework/ComponentInitFeedbackSupport`
- Shared feedback/banner contract:
  - `sap_ui5/service/framework/FeedbackBannerState`
  - `sap_ui5/service/framework/RuntimeInput`
- App controller action modules:
  - `sap_ui5/controller/support/AppControllerLifecycleActions`
  - `sap_ui5/controller/support/AppControllerOverlayActions`
  - `sap_ui5/controller/support/AppControllerShellActions`
  - `sap_ui5/controller/support/AppControllerStateActions`
  - `sap_ui5/controller/support/AppControllerDomActions`
- Detail validation summary support: `sap_ui5/controller/support/DetailValidationSummarySupport`
- Detail controller action modules:
  - `sap_ui5/controller/support/DetailChecklistCoreSupport`
  - `sap_ui5/controller/support/DetailChecklistStateActions`
  - `sap_ui5/controller/support/DetailChecklistRowActions`
  - `sap_ui5/controller/support/DetailAttachmentLocationActions`

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
