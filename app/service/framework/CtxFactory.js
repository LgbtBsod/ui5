sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/ODataChecklistRepoAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/DictAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/WorkflowAnalyticsAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/PersonSuggestAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LocationLookupAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/Ui5StateAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/TelemetryAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/ClockAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/SmartControlsAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/BrowserCacheAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LastChangeSetAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheValidationUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheReadUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/cache/usecases/CacheWriteUseCase"
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

    function safeBuild(vBuilder, mArgs) {
        try {
            if (!vBuilder) {
                return null;
            }
            if (typeof vBuilder.create === "function") {
                return vBuilder.create(mArgs || {});
            }
            return vBuilder;
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
            shell: ControllerModelRuntime.shell(oController),
            selected: ControllerModelRuntime.selected(oController),
            snapshot: ControllerModelRuntime.snapshot(oController),
            masterData: ControllerModelRuntime.masterData(oController),
            search: ControllerModelRuntime.named(oController, "search", true),
            env: ControllerModelRuntime.env(oController)
        };
    }

    function buildCtx(oController, mViewRefs) {
        var mModels = collectModels(oController);
        var oUiStateAdapter;

        // Historical name only: `uiState` in the context is a facade adapter over
        // normalized named models (`state`, `selected`, `snapshot`, `view`, etc.).
        // It is not a business-data owner and must stay tolerant to missing legacy
        // `uiState` JSONModel instances.
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
            cache: safeBuild(BrowserCacheAdapter, {
                stateModel: mModels.state
            }),
            lastChangeSet: safeBuild(LastChangeSetAdapter),
            cacheValidation: CacheValidationUseCase(),
            cacheRead: CacheReadUseCase(),
            cacheWrite: CacheWriteUseCase()
        };
    }

    return {
        buildCtx: buildCtx
    };
});
