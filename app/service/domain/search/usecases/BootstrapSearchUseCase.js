sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/framework/Effects",
    "sap_ui5/service/domain/shared/StatePaths"
], function (UseCase, Result, Effects, StatePaths) {
    "use strict";

    function BootstrapSearchUseCase() {
        UseCase.call(this, "BootstrapSearchUseCase");
    }

    BootstrapSearchUseCase.prototype = Object.create(UseCase.prototype);
    BootstrapSearchUseCase.prototype.constructor = BootstrapSearchUseCase;

    BootstrapSearchUseCase.prototype.execute = function (mInput, mCtx) {
        return Promise.resolve(Result.ok({ reason: (mInput && mInput.reason) || "bootstrap" }, [
            Effects.modelPatch("state", StatePaths.WORKFLOW_SEARCH_MODE, "EXACT"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_SEARCH_SEGMENTS, {
                checksFailSegment: "ALL",
                barriersFailSegment: "ALL"
            }),
            Effects.modelPatch("state", StatePaths.UI_BUSY_SEARCH_TABLE, false),
            Effects.modelPatch("view", "/bootstrapBusy", false)
        ])).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_SEARCH_TABLE, false),
                Effects.modelPatch("view", "/bootstrapBusy", false)
            ]);
        });
    };

    return BootstrapSearchUseCase;
});
