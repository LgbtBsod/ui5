sap.ui.define([
    "sap_ui5/service/usecase/DetailLifecycleUseCase"
], function (DetailLifecycleUseCase) {
    "use strict";

    function applySaveSuccess(mArgs) {
        var oSaved = mArgs.result.savedChecklist || {};
        var sSavedId = String((((oSaved || {}).root || {}).id) || "").trim();

        mArgs.dataModel.setProperty("/checkLists", mArgs.result.checkLists);
        mArgs.dataModel.setProperty("/visibleCheckLists", mArgs.result.checkLists);
        mArgs.dataModel.setProperty("/selectedChecklist", oSaved);
        mArgs.selectedModel.setData(oSaved);
        if (mArgs.stateModel && typeof mArgs.stateModel.setProperty === "function") {
            mArgs.stateModel.setProperty("/activeObjectId", sSavedId || null);
            mArgs.stateModel.setProperty("/copySourceId", null);
            mArgs.stateModel.setProperty("/objectAction", "");
        }

        DetailLifecycleUseCase.keepEditModeAfterSave(mArgs.stateModel);

        if (typeof mArgs.dispatchFullSave === "function") {
            mArgs.dispatchFullSave();
        }
        if (typeof mArgs.showSavedToast === "function") {
            mArgs.showSavedToast();
        }
        if (typeof mArgs.navigateToSaved === "function" && sSavedId) {
            mArgs.navigateToSaved(sSavedId);
        }
    }

    return {
        applySaveSuccess: applySaveSuccess
    };
});
