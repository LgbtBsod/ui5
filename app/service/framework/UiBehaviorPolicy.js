sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorConstants"
], function (UiBehaviorConstants) {
    "use strict";

    function uiScope() {
        return sap.ui.requireSync("PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes").ui;
    }

    function runSyncOperation(sOperation, mContext) {
        return uiScope().executeSync(sOperation, mContext || {});
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
        registerBehaviorOverride: function () {
            return uiScope().registerBehaviorOverride.apply(null, arguments);
        },
        unregisterBehaviorOverride: function () {
            return uiScope().unregisterBehaviorOverride.apply(null, arguments);
        },
        clearBehaviorOverrides: function () {
            return uiScope().clearBehaviorOverrides.apply(null, arguments);
        }
    };
});
