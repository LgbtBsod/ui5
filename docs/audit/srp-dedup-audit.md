# SRP and Dedup Audit (2026-03-05)

## Scope
- Full inventory generated for runtime modules and scripts.
- Controller-layer review for orchestration-only compliance.
- Dedup review for repeated behavior paths (UI feedback, row ops, drag/drop, export).

## Inventory
- Source of truth: `docs/audit/system-module-script-inventory.json`
- Module files indexed: 215
- Script files indexed: 152
- Controller files indexed: 28
- Service files indexed: 79
- View XML files indexed: 19

## Highest Concentration Hotspots
- `service/framework/ComponentInitRuntime.js` (490 lines)
- `controller/support/SearchViewSupport.js` (468 lines)
- `controller/support/DetailViewSupport.js` (357 lines)
- `controller/support/DetailAttachmentLocationActions.js` (162 lines)
- `controller/support/AppControllerShellActions.js` (192 lines)

## Completed Dedup/SRP Actions
- App controller is orchestration-only and delegates implementation to support actions.
  - `controller/App.controller.js`
  - `controller/support/AppControllerActions.js`
- Search controller is orchestration-only and delegates implementation to support actions.
  - `controller/Search.controller.js`
  - `controller/support/SearchControllerActions.js`
- Drag/drop attachment behavior centralized into single runtime implementation.
  - `controller/support/AttachmentDropZoneRuntime.js`
  - `controller/support/AttachmentUploadSupport.js`
- Row operations (`check`/`barrier`) unified into one configuration-driven flow.
  - `service/domain/detail/usecases/RowOpsUseCase.js`
- Effect handling unified into one dispatcher and shared prompt/dialog path.
  - `service/framework/EffectApplier.js`
- Export flow normalized via dedicated facade for consistent behavior across entry points.
  - `service/domain/search/ExportFacade.js`
  - `service/domain/search/SearchFacade.js`
- Component lock-release beacon logic extracted into dedicated support module (SRP for lifecycle vs transport/payload logic).
  - `service/framework/ComponentLockReleaseSupport.js`
  - `Component.js`
- ModelFactory construction flow deduplicated through shared model/default creators.
  - `model/ModelFactory.js`
- Manager imports consolidated through a single facade to keep component bootstrap modular and consistent.
  - `manager/ManagerFacade.js`
  - `Component.js`
  - `service/framework/ComponentInitRuntime.js`
- Attachment upload policy deduplicated to one canonical implementation (`util` layer), proxy duplicate removed.
  - `util/AttachmentUploadPolicy.js`
  - `controller/support/AttachmentUploadCore.js`
- QA pipeline validator list and optional validators centralized into single config source.
  - `scripts/lib/qa-pipeline-config.js`
  - `scripts/qa-all.js`
- Legacy browser smoke layer removed; coverage migrated to facade contract smoke.
  - `scripts/browser-smoke-domain-facade-contract.py`
  - `scripts/gateway-only-smoke-pack.py`
- Detail validation summary logic extracted from controller action hub into dedicated bounded support.
  - `controller/support/DetailValidationSummarySupport.js`
  - `controller/support/DetailControllerActions.js`
- Component init feedback/correlation/banner logic extracted into dedicated bounded support.
  - `service/framework/ComponentInitFeedbackSupport.js`
  - `service/framework/ComponentInitRuntime.js`
- Component init runtime split into bounded supports for save-guard, lock-runtime, listeners and boot.
  - `service/framework/ComponentInitSaveGuardSupport.js`
  - `service/framework/ComponentInitLockRuntimeSupport.js`
  - `service/framework/ComponentInitListenersSupport.js`
  - `service/framework/ComponentInitBootSupport.js`
  - `service/framework/ComponentInitRuntime.js`
- Detail controller action hub decomposed into bounded support modules (controller remains orchestrator).
  - `controller/support/DetailActionConstants.js`
  - `controller/support/DetailActionViewportSupport.js`
  - `controller/support/DetailActionPinnedRailSupport.js`
  - `controller/support/DetailActionDialogSupport.js`
  - `controller/support/DetailChecklistCoreSupport.js`
  - `controller/support/DetailChecklistStateActions.js`
  - `controller/support/DetailChecklistRowActions.js`
  - `controller/support/DetailAttachmentLocationActions.js`
  - `controller/support/DetailControllerActions.js` (composition only)
- App controller action hub decomposed into bounded support modules (controller remains orchestrator).
  - `controller/support/AppControllerLifecycleActions.js`
  - `controller/support/AppControllerOverlayActions.js`
  - `controller/support/AppControllerShellActions.js`
  - `controller/support/AppControllerStateActions.js`
  - `controller/support/AppControllerDomActions.js`
  - `controller/support/AppControllerActions.js` (composition only)
- Feedback/banner state creation and runtime input normalization centralized for identical behavior across component runtime, effect pipeline and controller mixins.
  - `service/framework/FeedbackBannerState.js`
  - `service/framework/RuntimeInput.js`
  - `service/framework/ComponentInitFeedbackSupport.js`
  - `service/framework/EffectApplier.js`
  - `controller/base/EffectMixin.js`
- QA gates modernized for modular architecture (support-module composition) and stale exceptions removed.
  - `scripts/final-static-qa.js`
  - `scripts/smart-odata-contract-gate.js`
  - `scripts/lib/finalArchitectureFreezeCore.js`
  - `scripts/controller-purity-gate.js`

## Controller Responsibility Status
- `controller/App.controller.js`: orchestration-only (delegates to `AppControllerActions`).
- `controller/Search.controller.js`: orchestration-only (delegates to `SearchControllerActions`).
- `controller/Detail.controller.js`: orchestration-only (delegates lifecycle to `DetailControllerLifecycle` and actions to `DetailControllerActions`).

## Next SRP Refactor Queue
- Split `DetailControllerActions` into bounded domains:
  - validation/lifecycle
  - table row actions
  - attachment actions
  - value-help actions
- Split `SearchViewSupport` into:
  - smart table binding policy
  - analytics rail lifecycle
  - route/search state sync

## Behavior Uniformity Guarantees (Current)
- Single effect pipeline for toast/banner/dialog/confirm.
- Single export pipeline through Search facade and Export facade.
- Single drag/drop attachment pipeline.
- Single row-ops rule set for checks and barriers.

## Validation
- Full QA pipeline executed after refactor.
- Result: PASS (41/41 gates).
