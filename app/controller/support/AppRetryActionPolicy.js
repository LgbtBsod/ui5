sap.ui.define([
    "checklist/app/service/framework/ActionContract",
    "checklist/app/service/framework/SecurityTokenRefresh",
    "checklist/app/service/framework/EffectActionRouting"
], function (ActionContract, SecurityTokenRefresh, EffectActionRouting) {
    "use strict";

    function runSaveRetry(oController) {
        return EffectActionRouting.dispatchByName(
            oController,
            null,
            ActionContract.ACTIONS.DETAIL_RETRY_GUARDED_SAVE,
            {}
        );
    }

    function runSearchRetry(oController) {
        var oSearchView = oController.byId && oController.byId("searchPaneHost");
        var oSearchController = oSearchView && oSearchView.getController && oSearchView.getController();
        if (oSearchController && typeof oSearchController.onRetrySearchLoad === "function") {
            return oSearchController.onRetrySearchLoad();
        }
        return Promise.resolve();
    }

    function runSessionRetry(oController) {
        var oOwner = oController.getOwnerComponent && oController.getOwnerComponent();
        var oModel = oOwner && oOwner.getModel && oOwner.getModel();
        return SecurityTokenRefresh.refresh(oModel);
    }

    function runRetry(oController, vRetryAction) {
        var sAction = ActionContract.normalizeRetryAction(vRetryAction);
        var mRetryHandlers = {};
        mRetryHandlers[ActionContract.RETRY_ACTIONS.SAVE] = runSaveRetry;
        mRetryHandlers[ActionContract.RETRY_ACTIONS.SEARCH] = runSearchRetry;
        mRetryHandlers[ActionContract.RETRY_ACTIONS.SESSION] = runSessionRetry;
        if (typeof mRetryHandlers[sAction] !== "function") {
            return Promise.resolve();
        }
        return mRetryHandlers[sAction](oController);
    }

    return {
        runRetry: runRetry
    };
});
