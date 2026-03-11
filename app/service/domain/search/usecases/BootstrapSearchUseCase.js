sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (UseCase, Result, Effects, StatePaths) {
    "use strict";

    function BootstrapSearchUseCase() {
        UseCase.call(this, "BootstrapSearchUseCase");
    }

    BootstrapSearchUseCase.prototype = Object.create(UseCase.prototype);
    BootstrapSearchUseCase.prototype.constructor = BootstrapSearchUseCase;

    BootstrapSearchUseCase.prototype.execute = function (mInput, mCtx) {
        var sReadyAt = new Date().toISOString();
        return Promise.resolve(Result.ok({ reason: (mInput && mInput.reason) || "bootstrap" }, [
            Effects.modelPatch("state", StatePaths.WORKFLOW_SEARCH_MODE, "EXACT"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_SEARCH_SEGMENTS, {
                checksFailSegment: "ALL",
                barriersFailSegment: "ALL"
            }),
            Effects.modelPatch("state", StatePaths.UI_BUSY_SEARCH_TABLE, false),
            Effects.modelPatch("state", StatePaths.READINESS_SEARCH, {
                status: "ready",
                ready: true,
                readyAt: sReadyAt,
                error: ""
            }),
            Effects.modelPatch("view", "/bootstrapBusy", false)
        ])).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_SEARCH_TABLE, false),
                Effects.modelPatch("state", StatePaths.READINESS_SEARCH, {
                    status: "error",
                    ready: false,
                    readyAt: "",
                    error: String((oError && oError.message) || "search_bootstrap_failed")
                }),
                Effects.modelPatch("view", "/bootstrapBusy", false)
            ]);
        });
    };

    return BootstrapSearchUseCase;
});
