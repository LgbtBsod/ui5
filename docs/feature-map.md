# Feature Map

## search
- owner: service/domain/search/
- model/search/SearchState.js
- service/domain/search/ExportFacade.js
- service/domain/search/SearchFacade.js
- service/domain/search/SearchSelectionEffects.js
- service/domain/search/usecases/AnalyticsUseCase.js
- service/domain/search/usecases/ApplyRebindPolicyUseCase.js
- service/domain/search/usecases/BootstrapSearchUseCase.js
- service/domain/search/usecases/BuildSearchFilterUseCase.js
- service/domain/search/usecases/ExecuteSearchUseCase.js
- service/domain/search/usecases/ExportSearchUseCase.js
- service/domain/search/usecases/RebindSearchUseCase.js
- service/domain/search/usecases/SelectRowUseCase.js
- service/domain/search/usecases/SelectionChangedUseCase.js
- util/search/RebindDebouncePolicy.js
- util/search/SearchBindingPolicy.js
- util/search/SearchFilterBuilder.js
- util/search/SearchMaxResults.js

## detail
- owner: service/domain/detail/
- service/domain/detail/AttachmentEffectSupport.js
- service/domain/detail/DetailAuthorizationSupport.js
- service/domain/detail/DetailFacade.js
- service/domain/detail/DetailSaveRuntimeSupport.js
- service/domain/detail/DetailStateAccess.js
- service/domain/detail/DetailValidationSupport.js
- service/domain/detail/usecases/AttachmentDeleteUseCase.js
- service/domain/detail/usecases/AttachmentUploadUseCase.js
- service/domain/detail/usecases/AutosaveDetailUseCase.js
- service/domain/detail/usecases/ChangeStatusUseCase.js
- service/domain/detail/usecases/CloseDetailUseCase.js
- service/domain/detail/usecases/DeleteChecklistUseCase.js
- service/domain/detail/usecases/EnterEditUseCase.js
- service/domain/detail/usecases/ForceReadOnlyUseCase.js
- service/domain/detail/usecases/LockLostUseCase.js
- service/domain/detail/usecases/OpenDetailUseCase.js
- service/domain/detail/usecases/PersonSuggestUseCase.js
- service/domain/detail/usecases/ResolveConflictUseCase.js
- service/domain/detail/usecases/RowOpsUseCase.js
- service/domain/detail/usecases/SaveDetailUseCase.js

## lock
- owner: service/domain/lock/
- architecture/air-traffic/deadlock-detector.js
- facades/LockFacade.js
- infra/adapters/ClockAdapter.js
- infra/adapters/LockAdapter.js
- manager/LockStatusMonitor.js
- ports/ClockPort.js
- ports/LockPort.js
- scripts/ci/edit-requires-lock-gate.js
- scripts/gates/autosave-lock-guard-gate.js
- scripts/gates/lock-state-enum-gate.js
- scripts/gates/no-islocked-writes-gate.js
- scripts/test-pack/smoke-lock.js
- service/domain/lock/usecases/TakeoverLockUseCase.js
- service/framework/ComponentInitLockRuntimeSupport.js
- service/framework/ComponentLockReleaseSupport.js

## autosave
- owner: service/domain/autosave/
- manager/AutoSaveCoordinator.js
- scripts/ci/autosave-input-contract-gate.js

## cache
- owner: service/domain/cache/
- service/domain/cache/ports/BrowserCachePort.js
- service/domain/cache/ports/LastChangeSetPort.js
- service/domain/cache/usecases/CacheReadUseCase.js
- service/domain/cache/usecases/CacheValidationUseCase.js
- service/domain/cache/usecases/CacheWriteUseCase.js

## dictionary
- owner: service/domain/dictionary/
- architecture/digital-twin/risk-predictor.js
- infra/adapters/DictAdapter.js
- ports/DictPort.js
- service/domain/shared/usecases/EnsureDictLoadedUseCase.js
- udos/court/verdict-engine.js

## personSuggest
- owner: service/domain/person/
- architecture/digital-twin/safe-split-suggester.js
- controller/support/DetailPersonInputSupport.js
- infra/adapters/PersonSuggestAdapter.js
- ports/PersonSuggestPort.js
- scripts/suggest-on-interaction-only-gate.js

## shared
- owner: service/domain/shared/
- architecture/air-traffic/conflict-detector.js
- architecture/air-traffic/merge-coordinator.js
- architecture/air-traffic/queue-manager.js
- architecture/air-traffic/scheduler.js
- architecture/air-traffic/traffic-dashboard.js
- architecture/digital-twin/impact-simulator.js
- architecture/digital-twin/patch-sandbox.js
- architecture/digital-twin/preflight.js
- architecture/digital-twin/report-generator.js
- architecture/digital-twin/twin-builder.js
- control/AppShellHeader.js
- control/ThemeToggle.js
- controller/App.controller.js
- controller/Base.controller.js
- controller/Detail.controller.js
- controller/Search.controller.js
- controller/TestUserDialog.controller.js
- controller/base/ControllerTextRuntime.js
- controller/base/EffectMixin.js
- controller/base/ModelAccessMixin.js

