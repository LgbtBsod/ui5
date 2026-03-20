sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsStateConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FacadeCommandConstants"
], function (UseCase, Result, Effects, StatePaths, AnalyticsStateConstants, FacadeCommandConstants) {
    "use strict";

    function InitializeSearchUseCase() {
        UseCase.call(this, "InitializeSearchUseCase");
    }

    InitializeSearchUseCase.prototype = Object.create(UseCase.prototype);
    InitializeSearchUseCase.prototype.constructor = InitializeSearchUseCase;

    InitializeSearchUseCase.prototype.execute = function (mInput, mCtx) {
        var sReadyAt = new Date().toISOString();
        return Promise.resolve(Result.ok({ reason: (mInput && mInput.reason) || FacadeCommandConstants.SEARCH.BOOTSTRAP }, [
            Effects.modelPatch("state", StatePaths.WORKFLOW_SEARCH_MODE, "EXACT"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_SEARCH_SEGMENTS, {
                checksFailSegment: "ALL",
                barriersFailSegment: "ALL"
            }),
            Effects.modelPatch("state", StatePaths.UI_BUSY_SEARCH_TABLE, false),
            Effects.modelPatch("state", StatePaths.READINESS_SEARCH, {
                status: AnalyticsStateConstants.LOAD_STATUS.READY,
                ready: true,
                readyAt: sReadyAt,
                error: ""
            }),
            Effects.modelPatch("view", "/bootstrapBusy", false)
        ])).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_SEARCH_TABLE, false),
                Effects.modelPatch("state", StatePaths.READINESS_SEARCH, {
                    status: AnalyticsStateConstants.LOAD_STATUS.ERROR,
                    ready: false,
                    readyAt: "",
                    error: String((oError && oError.message) || "search_bootstrap_failed")
                }),
                Effects.modelPatch("view", "/bootstrapBusy", false)
            ]);
        });
    };

    return InitializeSearchUseCase;
});
