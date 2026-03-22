sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorConstants"
], function (BehaviorScopes, UiBehaviorConstants) {
    "use strict";

    function runSyncOperation(sOperation, mContext) {
        return BehaviorScopes.ui.executeSync(sOperation, mContext || {});
    }

    function create(mOptions) {
        var oStateModel = mOptions && mOptions.stateModel;
        var fnResolveText = mOptions && mOptions.resolveText;

        return {
            setGlobalBusy: function (bBusy) {
                return runSyncOperation(UiBehaviorConstants.OP_SET_GLOBAL_BUSY, {
                    stateModel: oStateModel,
                    busy: bBusy
                });
            },
            setSearchBusy: function (bBusy) {
                return runSyncOperation(UiBehaviorConstants.OP_SET_SEARCH_BUSY, {
                    stateModel: oStateModel,
                    busy: bBusy
                });
            },
            setDetailBusy: function (bBusy) {
                return runSyncOperation(UiBehaviorConstants.OP_SET_DETAIL_BUSY, {
                    stateModel: oStateModel,
                    busy: bBusy
                });
            },
            setGlobalBanner: function (mInput) {
                return runSyncOperation(UiBehaviorConstants.OP_SET_GLOBAL_BANNER, {
                    stateModel: oStateModel,
                    bannerInput: mInput || {},
                    resolveText: fnResolveText
                });
            },
            clearGlobalBanner: function () {
                return runSyncOperation(UiBehaviorConstants.OP_CLEAR_GLOBAL_BANNER, {
                    stateModel: oStateModel,
                    resolveText: fnResolveText
                });
            }
        };
    }

    return {
        create: create,
        registerBehaviorOverride: BehaviorScopes.ui.registerBehaviorOverride,
        unregisterBehaviorOverride: BehaviorScopes.ui.unregisterBehaviorOverride,
        clearBehaviorOverrides: BehaviorScopes.ui.clearBehaviorOverrides
    };
});
