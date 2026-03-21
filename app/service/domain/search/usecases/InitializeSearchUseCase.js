sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsStateConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FacadeCommandConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchRuntimeConstants"
], function (Result, Effects, StatePaths, AnalyticsStateConstants, FacadeCommandConstants, ModelContracts, SearchRuntimeConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var SEARCH_MODE = SearchRuntimeConstants.SEARCH_MODE;
    var SEARCH_SEGMENTS = SearchRuntimeConstants.SEARCH_SEGMENTS;

    function InitializeSearchUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var sReadyAt = new Date().toISOString();
        return Promise.resolve(Result.ok({ reason: (mInput && mInput.reason) || FacadeCommandConstants.SEARCH.BOOTSTRAP }, [
            Effects.modelPatch(STATE_MODEL, StatePaths.SEARCH_MODE, SEARCH_MODE.EXACT),
            Effects.modelPatch(STATE_MODEL, StatePaths.SEARCH_CHECKS_FAIL_SEGMENT, SEARCH_SEGMENTS.ALL),
            Effects.modelPatch(STATE_MODEL, StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT, SEARCH_SEGMENTS.ALL),
            Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_SEARCH_TABLE, false),
            Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_SEARCH, {
                status: AnalyticsStateConstants.LOAD_STATUS.READY,
                ready: true,
                readyAt: sReadyAt,
                error: ""
            }),
            Effects.modelPatch(VIEW_MODEL, "/bootstrapBusy", false)
        ])).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch(STATE_MODEL, StatePaths.UI_BUSY_SEARCH_TABLE, false),
                Effects.modelPatch(STATE_MODEL, StatePaths.READINESS_SEARCH, {
                    status: AnalyticsStateConstants.LOAD_STATUS.ERROR,
                    ready: false,
                    readyAt: "",
                    error: String((oError && oError.message) || "search_bootstrap_failed")
                }),
                Effects.modelPatch(VIEW_MODEL, "/bootstrapBusy", false)
            ]);
        });
    }

    return InitializeSearchUseCase;
});
