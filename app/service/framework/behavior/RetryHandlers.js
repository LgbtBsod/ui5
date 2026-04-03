sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants"
], function (ActionContract, RuntimeInput, BehaviorRegistry, MessageCodeConstants) {
    "use strict";

    var RETRY_SCOPE = "retry";
    var bDefaultsRegistered = false;
    var TECHNICAL_CODES = MessageCodeConstants.TECHNICAL;

    function dispatchByName(oOptions, sActionName, oPayload) {
        var oDispatcher = oOptions && oOptions.actionDispatcher;
        var sResolvedAction = RuntimeInput.asString(sActionName).trim();
        if (!oDispatcher || typeof oDispatcher.dispatch !== "function" || !sResolvedAction) {
            return Promise.resolve(false);
        }
        return oDispatcher.dispatch(sResolvedAction, oPayload || {});
    }

    function runSaveRetry(mContext) {
        return dispatchByName(
            mContext,
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
            return Promise.reject(new Error(TECHNICAL_CODES.SECURITY_TOKEN_REFRESH_UNAVAILABLE));
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
