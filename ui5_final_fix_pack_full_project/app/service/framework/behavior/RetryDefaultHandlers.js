sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectActionRouting",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SecurityTokenRefresh",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (ActionContract, EffectActionRouting, SecurityTokenRefresh, BehaviorRegistry) {
    "use strict";

    var RETRY_SCOPE = "retry";
    var bDefaultsRegistered = false;

    function runSaveRetry(mContext) {
        return EffectActionRouting.dispatchByName(
            mContext.controller,
            null,
            ActionContract.ACTIONS.DETAIL_RETRY_GUARDED_SAVE,
            {}
        );
    }

    function runSearchRetry(mContext) {
        var oSearchView = mContext.controller && mContext.controller.byId && mContext.controller.byId("searchPaneHost");
        if (oSearchView && typeof oSearchView.getItems === "function") {
            oSearchView = (oSearchView.getItems() || [])[0] || null;
        }
        var oSearchController = oSearchView && oSearchView.getController && oSearchView.getController();
        if (oSearchController && typeof oSearchController.onRetrySearchLoad === "function") {
            return oSearchController.onRetrySearchLoad();
        }
        return Promise.resolve();
    }

    function runSessionRetry(mContext) {
        var oOwner = mContext.controller && mContext.controller.getOwnerComponent && mContext.controller.getOwnerComponent();
        var oModel = oOwner && oOwner.getModel && oOwner.getModel();
        return SecurityTokenRefresh.refresh(oModel);
    }

    var mHandlers = {
        save: runSaveRetry,
        search: runSearchRetry,
        session: runSessionRetry
    };

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(mHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(RETRY_SCOPE, sOperation, mHandlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        handlers: mHandlers,
        ensureRegistered: ensureRegistered
    };
});
