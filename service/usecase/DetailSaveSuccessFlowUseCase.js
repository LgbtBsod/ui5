sap.ui.define([
    "sap_ui5/service/usecase/DetailLifecycleUseCase"
], function (DetailLifecycleUseCase) {
    "use strict";

    function applySaveSuccess(mArgs) {
        mArgs.dataModel.setProperty("/checkLists", mArgs.result.checkLists);
        mArgs.dataModel.setProperty("/visibleCheckLists", mArgs.result.checkLists);
        mArgs.dataModel.setProperty("/selectedChecklist", mArgs.result.savedChecklist);
        mArgs.selectedModel.setData(mArgs.result.savedChecklist);

        DetailLifecycleUseCase.keepEditModeAfterSave(mArgs.stateModel);

        if (typeof mArgs.dispatchFullSave === "function") {
            mArgs.dispatchFullSave();
        }
        if (typeof mArgs.showSavedToast === "function") {
            mArgs.showSavedToast();
        }
    }

    return {
        applySaveSuccess: applySaveSuccess
    };
});
