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
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LastChangeSetAdapter"
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
    LastChangeSetAdapter
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

    function buildStateAdapters(mModels, mViewRefs) {
        var oUiStateAdapter = safeBuild(Ui5StateAdapter, mModels);

        return {
            uiState: oUiStateAdapter,
            stateModel: mModels.state,
            smartControls: mViewRefs ? safeBuild(SmartControlsAdapter, mViewRefs) : null
        };
    }

    function buildInfraAdapters(mModels, mStateAdapters) {
        return {
            repo: safeBuild(ODataChecklistRepoAdapter, {
                uiState: mStateAdapters.uiState,
                stateModel: mModels.state
            }),
            lock: safeBuild(LockAdapter, {
                uiState: mStateAdapters.uiState,
                stateModel: mModels.state
            }),
            dict: safeBuild(DictAdapter, {
                masterDataModel: mModels.masterData,
                stateModel: mModels.state
            }),
            analytics: safeBuild(WorkflowAnalyticsAdapter),
            personSuggest: safeBuild(PersonSuggestAdapter),
            locationLookup: safeBuild(LocationLookupAdapter),
            telemetry: safeBuild(TelemetryAdapter, {
                stateModel: mModels.state
            }),
            clock: safeBuild(ClockAdapter),
            cache: safeBuild(BrowserCacheAdapter, {
                stateModel: mModels.state
            }),
            lastChangeSet: safeBuild(LastChangeSetAdapter)
        };
    }

    return {
        buildStateAdapters: buildStateAdapters,
        buildInfraAdapters: buildInfraAdapters
    };
});
