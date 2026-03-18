sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiBehaviorDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiBehaviorOverrideHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiBehaviorConstants"
], function (BehaviorResolver, UiBehaviorDefaultHandlers, UiBehaviorOverrideHandlers, UiBehaviorConstants) {
    "use strict";

    function runSyncOperation(sOperation, mContext) {
        UiBehaviorDefaultHandlers.ensureRegistered();
        UiBehaviorOverrideHandlers.ensureRegistered();
        return BehaviorResolver.executeSync(
            UiBehaviorConstants.SCOPE,
            sOperation,
            mContext || {},
            UiBehaviorDefaultHandlers.handlers
        );
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
        registerBehaviorOverride: UiBehaviorOverrideHandlers.register,
        unregisterBehaviorOverride: UiBehaviorOverrideHandlers.unregister,
        clearBehaviorOverrides: UiBehaviorOverrideHandlers.clear
    };
});
