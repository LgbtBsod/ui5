sap.ui.define([
    "sap_ui5/service/usecase/DetailSaveOrchestrationUseCase"
], function (DetailSaveOrchestrationUseCase) {
    "use strict";

    function runSaveFlow(mDeps) {
        return mDeps.runWithLoading(function () {
            return DetailSaveOrchestrationUseCase.runSaveFlow({
                saveChecklist: mDeps.saveChecklist,
                loadChecklistCollection: mDeps.loadChecklistCollection,
                applySaveResult: mDeps.applySaveResult,
                handleSaveError: mDeps.handleSaveError
            });
        }).then(function (oResult) {
            return { ok: true, reason: "saved", result: oResult };
        });
    }

    return {
        runSaveFlow: runSaveFlow
    };
});
