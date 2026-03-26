# Project Structure Map

## Stable Owners
- `app/controller/App.controller.js`: app shell and shell UI orchestration
- `app/controller/Search.controller.js`: search UI owner
- `app/controller/Analytics.controller.js`: analytics UI owner
- `app/constants/DetailContracts.js`: detail-domain constants only
- `app/constants/MessageKeyConstants.js`: frontend i18n keys only
- `app/constants/MessageCodeConstants.js`: frontend machine-readable codes only
- `app/service/framework/ComponentBootstrap.js`: bootstrap assembly plus manifest/main-service model bootstrap
- `scripts/lock-contract-naming-gate.js`: lock naming consistency gate across metadata, frontend adapter, and ABAP DPC boundary
- UI5 Core APIs are consumed directly in runtime/controller owners; `Ui5RuntimeFacade.js` is removed
- `app/service/framework/CtxRuntimeFactory.js`: runtime context assembly owner
- `app/service/runtime/component/ComponentLifecycleRuntime.js`: runtime attach + boot sequence owner
- `app/service/runtime/component/ComponentModelInitRuntime.js`: model bootstrap plus internal runtime state owner
- `app/controller/search/SearchActionBehavior.js`: search action orchestration plus local busy-wrapper owner
- `app/controller/detail/DetailValidationSummaryRuntime.js`: validation summary owner plus local validation helper logic
- `app/controller/detail/DetailControllerRuntime.js`: detail controller runtime owner plus local dialog/viewport helper logic
- `app/service/framework/behavior/NavigationDefaultHandlers.js`: navigation default handler owner plus direct workspace navigation wiring
- `app/service/framework/behavior/UiDecisionDefaultHandlers.js`: ui decision default handler owner plus local toast/error/confirm helper logic
- UI decision consumers call `UiDecisionDefaultHandlers` directly; the extra coordinator layer is removed
- `app/service/runtime/component/ComponentSaveGuardRuntime.js`: guarded-save owner

## Deleted Transitional Layers
- `app/service/runtime/component/ComponentMainServiceRuntime.js`
- `app/service/runtime/component/ComponentModelBootstrap.js`
- `app/service/runtime/component/ComponentGuardedSaveRuntime.js`
- `app/service/runtime/component/ComponentRuntimeOptionsFactory.js`
- `app/service/runtime/component/ComponentBootstrapDependencyBuilder.js`
- `app/service/framework/Ui5RuntimeFacade.js`
- `app/service/runtime/component/ComponentNavigationRuntime.js`
- `app/service/runtime/component/ComponentInternalRuntimeState.js`
- `app/service/framework/ControllerActionBusyRuntime.js`
- `app/controller/detail/internal/DetailValidationHelperRuntime.js`
- `app/controller/detail/DetailActionDialogRuntime.js`
- `app/controller/detail/DetailAdaptiveViewportRuntime.js`
- `app/service/framework/execution/behavior/NavigationBehaviorHelpers.js`
- `app/service/framework/execution/behavior/UiDecisionBehaviorHelpers.js`
- `app/service/framework/execution/UiDecisionCoordinator.js`
- `app/service/framework/CtxModelResolver.js`
- `app/service/framework/CtxCacheRuntimeFactory.js`

## Remaining Structural Hotspots
- `app/controller/detail/*`
- `app/controller/search/*`
- `app/service/framework/*`
- App shell orchestration now terminates in `app/controller/App.controller.js`; the extra `AppShellCoordinator.js` layer was removed.
- Attachment upload ingress now terminates in `app/infra/adapters/shared/AttachmentRepoRuntime.js`; the extra shared `AttachmentValueCodec.js` layer was removed.
# Structure Map Delta

- `app/infra/adapters/shared/ODataChecklistMutationRuntime.js`: canonical function-import write adapter for aggregate mutations and copy flow.
- `scripts/lock-contract-naming-gate.js`: release gate for canonical `DB_KEY` lock/copy contract.
- `backend/mock_gateway/tests/test_lock_gateway_api_contract.py`: backend contract regression coverage for lock/copy naming.
