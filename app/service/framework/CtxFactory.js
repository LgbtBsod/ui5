sap.ui.define([
    "checklist/app/infra/adapters/ODataChecklistRepoAdapter",
    "checklist/app/infra/adapters/LockAdapter",
    "checklist/app/infra/adapters/DictAdapter",
    "checklist/app/infra/adapters/WorkflowAnalyticsAdapter",
    "checklist/app/infra/adapters/PersonSuggestAdapter",
    "checklist/app/infra/adapters/LocationLookupAdapter",
    "checklist/app/infra/adapters/Ui5StateAdapter",
    "checklist/app/infra/adapters/TelemetryAdapter",
    "checklist/app/infra/adapters/ClockAdapter",
    "checklist/app/infra/adapters/SmartControlsAdapter",
    "checklist/app/infra/adapters/BrowserCacheAdapter",
    "checklist/app/infra/adapters/LastChangeSetAdapter",
    "checklist/app/service/framework/ControllerModelRuntime",
    "checklist/app/service/domain/cache/usecases/CacheValidationUseCase",
    "checklist/app/service/domain/cache/usecases/CacheReadUseCase",
    "checklist/app/service/domain/cache/usecases/CacheWriteUseCase"
], function (
    ODataChecklistRepoAdapter,
    LockAdapter,
    DictAdapter,
    WorkflowAnalyticsAdapter,
    PersonSuggestAdapter,
    LocationLookupAdapter,
    Ui5StateAdapter,
    TelemetryAdapter,
    ClockAdapter,
    SmartControlsAdapter,
    BrowserCacheAdapter,
    LastChangeSetAdapter,
    ControllerModelRuntime,
    CacheValidationUseCase,
    CacheReadUseCase,
    CacheWriteUseCase
) {
    "use strict";

    function safeBuild(fnFactory, mArgs) {
        try {
            return fnFactory && fnFactory.create ? fnFactory.create(mArgs || {}) : null;
        } catch (e) {
            return null;
        }
    }

    function collectModels(oController) {
        if (!oController) {
            return {};
        }
        return {
            default: ControllerModelRuntime.defaultModel(oController),
            view: ControllerModelRuntime.viewState(oController),
            state: ControllerModelRuntime.state(oController),
            uiState: ControllerModelRuntime.uiState(oController),
            selected: ControllerModelRuntime.selected(oController),
            masterData: ControllerModelRuntime.masterData(oController),
            search: ControllerModelRuntime.named(oController, "search", true),
            env: ControllerModelRuntime.env(oController)
        };
    }

    function buildCtx(oController, mViewRefs) {
        var mModels = collectModels(oController);
        var oUiStateAdapter;

        if (!mModels.uiState) {
            throw new Error("CtxFactory: required model \"uiState\" is missing");
        }

        oUiStateAdapter = safeBuild(Ui5StateAdapter, mModels);

        return {
            repo: safeBuild(ODataChecklistRepoAdapter, {
                uiState: oUiStateAdapter,
                stateModel: mModels.state
            }),
            lock: safeBuild(LockAdapter, {
                uiState: oUiStateAdapter,
                stateModel: mModels.state
            }),
            dict: safeBuild(DictAdapter, {
                masterDataModel: mModels.masterData,
                stateModel: mModels.state,
                envModel: mModels.env
            }),
            analytics: safeBuild(WorkflowAnalyticsAdapter),
            personSuggest: safeBuild(PersonSuggestAdapter),
            locationLookup: safeBuild(LocationLookupAdapter),
            uiState: oUiStateAdapter,
            stateModel: mModels.state,
            telemetry: safeBuild(TelemetryAdapter, {
                stateModel: mModels.state
            }),
            clock: safeBuild(ClockAdapter),
            smartControls: (mViewRefs ? safeBuild(SmartControlsAdapter, mViewRefs) : null),
            cache: safeBuild(BrowserCacheAdapter),
            lastChangeSet: safeBuild(LastChangeSetAdapter),
            cacheValidation: new CacheValidationUseCase(),
            cacheRead: new CacheReadUseCase(),
            cacheWrite: new CacheWriteUseCase()
        };
    }

    return {
        buildCtx: buildCtx
    };
});
