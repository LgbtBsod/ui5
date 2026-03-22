sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (ModelStateRuntime, FeedbackBannerRuntime, BehaviorRegistry, UiBehaviorConstants, StatePaths) {
    "use strict";

    var bDefaultsRegistered = false;

    function writeBusy(mContext, sPath) {
        return ModelStateRuntime.writeOnModel(mContext.stateModel, sPath, !!mContext.busy);
    }

    var handlers = {};

    handlers[UiBehaviorConstants.OP_SET_GLOBAL_BUSY] = function (mContext) {
        return writeBusy(mContext || {}, StatePaths.UI_BUSY_GLOBAL);
    };

    handlers[UiBehaviorConstants.OP_SET_SEARCH_BUSY] = function (mContext) {
        return writeBusy(mContext || {}, StatePaths.UI_BUSY_SEARCH_TABLE);
    };

    handlers[UiBehaviorConstants.OP_SET_DETAIL_BUSY] = function (mContext) {
        return writeBusy(mContext || {}, StatePaths.UI_BUSY_DETAIL);
    };

    handlers[UiBehaviorConstants.OP_SET_GLOBAL_BANNER] = function (mContext) {
        var oContext = mContext || {};
        return FeedbackBannerRuntime.setBanner(
            oContext.stateModel,
            UiBehaviorConstants.BANNER_GLOBAL_ID,
            Object.assign({ scope: "global" }, oContext.bannerInput || {}),
            { resolveText: oContext.resolveText }
        );
    };

    handlers[UiBehaviorConstants.OP_CLEAR_GLOBAL_BANNER] = function (mContext) {
        return FeedbackBannerRuntime.clearBanner((mContext || {}).stateModel, UiBehaviorConstants.BANNER_GLOBAL_ID);
    };

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(handlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(UiBehaviorConstants.SCOPE, sOperation, handlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        defaults: {
            handlers: handlers,
            ensureRegistered: ensureRegistered
        },
        overrides: {
            ensureRegistered: function () {
                return true;
            },
            register: function (sId, fnHandler) {
                return BehaviorRegistry.registerOverride(UiBehaviorConstants.SCOPE, sId, fnHandler);
            },
            unregister: function (sId) {
                return BehaviorRegistry.unregisterOverride(UiBehaviorConstants.SCOPE, sId);
            },
            clear: function () {
                return BehaviorRegistry.clearOverrides(UiBehaviorConstants.SCOPE);
            }
        }
    };
});
