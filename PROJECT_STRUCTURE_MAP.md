# Project Structure Map

## Stable Owners
- `app/controller/App.controller.js`: app shell and shell UI orchestration
- `app/controller/Search.controller.js`: search UI owner
- `app/controller/Analytics.controller.js`: analytics UI owner
- `app/constants/DetailContracts.js`: detail-domain constants only
- `app/constants/MessageKeyConstants.js`: frontend i18n keys only
- `app/constants/MessageCodeConstants.js`: frontend machine-readable codes only
- `app/service/framework/ComponentBootstrap.js`: bootstrap assembly plus manifest/main-service model bootstrap
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
