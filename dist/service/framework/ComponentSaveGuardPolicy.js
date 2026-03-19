sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentSaveGuardContracts"
], function (ActionContract, FeedbackBannerRuntime, ModelStateRuntime, ComponentSaveGuardContracts) {
    "use strict";

    var BANNER_DETAIL = ComponentSaveGuardContracts.BANNER_DETAIL;
    var BANNER_LEVEL = ComponentSaveGuardContracts.BANNER_LEVEL;
    var BANNER_TEXT_KEY = ComponentSaveGuardContracts.BANNER_TEXT_KEY;
    var PATHS = ComponentSaveGuardContracts.PATHS;

    function buildSaveBannerPayload(oStateModel, mOptions) {
        var bSessionExpired = !!mOptions.sessionExpired;
        var bOffline = ModelStateRuntime.readOnModel(oStateModel, PATHS.NETWORK_ONLINE, true) === false;
        var sDetail = String(mOptions.detail || BANNER_DETAIL.SAVE_FAILED);
        return FeedbackBannerRuntime.createRetryBannerInput(
            bSessionExpired ? BANNER_LEVEL.ERROR : (bOffline ? BANNER_LEVEL.WARNING : BANNER_LEVEL.ERROR),
            bSessionExpired ? BANNER_TEXT_KEY.SESSION_EXPIRED : (bOffline ? BANNER_TEXT_KEY.NETWORK_UNAVAILABLE : BANNER_TEXT_KEY.OBJECT_SAVE_FAILED),
            {
                scope: "global",
                textArgs: bSessionExpired ? [] : [sDetail],
                details: sDetail,
                correlationId: String(mOptions.correlationId || ""),
                retryAction: ActionContract.RETRY_ACTIONS.SAVE,
                retryTextKey: BANNER_TEXT_KEY.RETRY_NOW
            }
        );
    }

    function buildWorkingBannerPayload() {
        return FeedbackBannerRuntime.createRetryBannerInput(BANNER_LEVEL.INFO, BANNER_TEXT_KEY.WORKING_LONG, {
            scope: "global",
            retryAction: ActionContract.RETRY_ACTIONS.SAVE,
            retryTextKey: BANNER_TEXT_KEY.RETRY_NOW
        });
    }

    return {
        buildSaveBannerPayload: buildSaveBannerPayload,
        buildWorkingBannerPayload: buildWorkingBannerPayload
    };
});
