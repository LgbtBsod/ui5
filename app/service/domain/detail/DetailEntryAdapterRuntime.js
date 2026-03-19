sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailWorkflowRuntime"
], function (DetailWorkflowRuntime) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function decorateEnterEdit(oPromise, mInput) {
        return Promise.resolve(oPromise).then(function (oResult) {
            return DetailWorkflowRuntime.decorateEnterEditResult(oResult, mInput);
        });
    }

    function buildDiscardResult(oUiState) {
        return Promise.resolve({
            ok: true,
            effects: DetailWorkflowRuntime.buildDiscardEffects(oUiState)
        });
    }

    return {
        buildCancelEnterEditResult: DetailWorkflowRuntime.buildCancelEnterEditResult,
        buildDiscardResult: buildDiscardResult,
        decorateEnterEdit: decorateEnterEdit,
        executeUseCase: executeUseCase
    };
});
