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

    function resolveDirectModel(oHost, sName) {
        if (!oHost || typeof oHost.getModel !== "function") {
            return null;
        }
        return oHost.getModel(sName);
    }

    function collectModels(oController) {
        var oView;
        if (!oController) {
            return {};
        }
        oView = ControllerModelRuntime.view(oController);
        return {
            default: ControllerModelRuntime.defaultModel(oController) || resolveDirectModel(oController),
            view: ControllerModelRuntime.viewState(oController) || (oView && oView.getModel ? oView.getModel("view") : null),
            state: ControllerModelRuntime.state(oController) || resolveDirectModel(oController, "state"),
            detail: ControllerModelRuntime.detail(oController) || resolveDirectModel(oController, "detail"),
            shell: ControllerModelRuntime.shell(oController) || resolveDirectModel(oController, "shell"),
            masterData: ControllerModelRuntime.masterData(oController) || resolveDirectModel(oController, "masterData")
        };
    }

    function buildCtx(oController, mViewRefs) {
        var mModels = collectModels(oController);
        var oUiStateAdapter;

        // Historical name only: `uiState` in the context is a facade adapter over
        // normalized named models (`state`, `detail`, `view`, etc.).
        // It is not a business-data owner and must stay tolerant to missing
        // component-local models.
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
                stateModel: mModels.state
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
