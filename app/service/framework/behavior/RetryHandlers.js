sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectActionRouting",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (ActionContract, EffectActionRouting, BehaviorRegistry) {
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
        var oFcl = mContext.controller && mContext.controller.byId && mContext.controller.byId("mainFcl");
        var oSearchView = oFcl && oFcl.getBeginColumnPages && (oFcl.getBeginColumnPages() || [])[0];
        var oSearchController = oSearchView && oSearchView.getController && oSearchView.getController();
        if (oSearchController && typeof oSearchController.onRetrySearchLoad === "function") {
            return oSearchController.onRetrySearchLoad();
        }
        return Promise.resolve();
    }

    function refreshSecurityToken(oModel) {
        if (!oModel || typeof oModel.refreshSecurityToken !== "function") {
            return Promise.reject(new Error("security_token_refresh_unavailable"));
        }
        return new Promise(function (resolve, reject) {
            oModel.refreshSecurityToken(function () { resolve(true); }, reject, true);
        });
    }

    function runSessionRetry(mContext) {
        var oOwner = mContext.controller && mContext.controller.getOwnerComponent && mContext.controller.getOwnerComponent();
        var oModel = oOwner && oOwner.getModel && oOwner.getModel();
        return refreshSecurityToken(oModel);
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
        defaults: {
            handlers: mHandlers,
            ensureRegistered: ensureRegistered
        },
        overrides: {
            ensureRegistered: function () {
                return true;
            },
            register: function (sId, fnHandler) {
                return BehaviorRegistry.registerOverride(RETRY_SCOPE, sId, fnHandler);
            },
            unregister: function (sId) {
                return BehaviorRegistry.unregisterOverride(RETRY_SCOPE, sId);
            },
            clear: function () {
                return BehaviorRegistry.clearOverrides(RETRY_SCOPE);
            }
        }
    };
});
