# Merge Delete Plan

## Completed In This Pass
- `app/service/framework/Ui5RuntimeFacade.js` -> direct `sap/ui/core/Core` usage in consuming owners
  Why: file only proxied stable public UI5 Core APIs and duplicated standard framework surface without adding domain value.
- `app/service/runtime/component/ComponentBootstrapDependencyBuilder.js` -> `app/service/framework/ComponentBootstrap.js`
  Why: file only grouped and flattened bootstrap dependencies for a single caller and added naming overhead without an independent runtime boundary.
- `app/service/runtime/component/ComponentMainServiceRuntime.js` -> `app/service/framework/ComponentBootstrap.js`
  Why: file only created the manifest-owned main service model and had no independent semantic boundary outside bootstrap.
- `app/service/runtime/component/ComponentModelBootstrap.js` -> `app/service/framework/ComponentBootstrap.js`
  Why: file only chained model initialization plus main-service creation and existed as a pass-through bootstrap layer.
- `app/service/runtime/component/ComponentGuardedSaveRuntime.js` -> `app/service/runtime/component/ComponentSaveGuardRuntime.js`
  Why: file only delegated `createHandler` into `createRunGuardedSave` and duplicated the same save-guard owner boundary.
- `app/service/framework/CtxModelResolver.js` -> `app/service/framework/CtxRuntimeFactory.js`
  Why: file only resolved models for a single consumer and had no stable boundary outside context assembly.
- `app/service/framework/CtxCacheRuntimeFactory.js` -> `app/service/framework/CtxRuntimeFactory.js`
  Why: file only wrapped three cache usecases for a single consumer and existed as a factory shim.
- `app/service/runtime/component/ComponentRuntimeOptionsFactory.js` -> `app/service/framework/ComponentBootstrap.js` and `app/service/runtime/component/ComponentLifecycleRuntime.js`
  Why: file only assembled runtime option objects for exactly two consumers and had no stable semantic boundary outside those owners.
- `app/service/runtime/component/ComponentNavigationRuntime.js` -> `app/service/runtime/component/ComponentLifecycleRuntime.js`
  Why: file only forwarded navigation-intent calls into `NavigationIntentService` for one consumer and had no standalone boundary.
- `app/service/runtime/component/ComponentInternalRuntimeState.js` -> `app/service/runtime/component/ComponentModelInitRuntime.js`
  Why: file only seeded and reset internal cache/env objects for one consumer and existed as bootstrap-local plumbing.
- `app/service/framework/ControllerActionBusyRuntime.js` -> `app/controller/search/SearchActionBehavior.js`
  Why: file only wrapped a single `withFlag` busy helper for one consumer and had no cross-feature semantic boundary.
- `app/controller/detail/internal/DetailValidationHelperRuntime.js` -> `app/controller/detail/DetailValidationSummaryRuntime.js`
  Why: file only hosted validation-summary-local path/value helpers for one consumer and had no standalone detail boundary.
- `app/controller/detail/DetailActionDialogRuntime.js` -> `app/controller/detail/DetailControllerRuntime.js`
  Why: file only contributed controller-local dialog focus and value-help timer helpers to one controller runtime owner.
- `app/controller/detail/DetailAdaptiveViewportRuntime.js` -> `app/controller/detail/DetailControllerRuntime.js`
  Why: file only contributed adaptive viewport sync logic to one controller runtime owner.
- `app/service/framework/execution/behavior/NavigationBehaviorHelpers.js` -> `app/service/framework/behavior/NavigationDefaultHandlers.js`
  Why: file only re-exported navigation calls into a single consumer and had no separate execution-layer boundary.
- `app/service/framework/execution/behavior/UiDecisionBehaviorHelpers.js` -> `app/service/framework/behavior/UiDecisionDefaultHandlers.js`
  Why: file only hosted helper calls for one default decision-handler owner and had no standalone behavior boundary.
- `app/service/framework/execution/UiDecisionCoordinator.js` -> direct consumer use of `app/service/framework/behavior/UiDecisionDefaultHandlers.js`
  Why: coordinator added an extra dispatch layer over a single canonical owner while override hooks were unused across the repo.

## Remaining High-Value Candidates
- thin wrappers in `app/controller/detail/*`
- pass-through orchestration in `app/controller/search/*`
- framework helper wrappers that only expose one forwarding function
- Completed:
  - `AppShellCoordinator.js` merged into `App.controller.js` because it only delegated init/theme/exit orchestration back to the controller.
  - `AttachmentValueCodec.js` merged into `AttachmentRepoRuntime.js` because it was a thin shared base64 wrapper without a valid domain boundary.
# Merge/Delete Delta

- No extra file deletions were executed in this pass because the remaining wrapper-sprawl candidates are still cross-wired through controller/runtime tests and require a broader semantic merge, not blind removal.
- No extra file merges were executed in this pass for the same reason; contract drift remediation was prioritized over risky structural churn.
