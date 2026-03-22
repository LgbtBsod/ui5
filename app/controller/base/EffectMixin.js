sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants"
], function (FeedbackCoordinator, FeedbackConstants) {
    "use strict";

    return {
        showI18nToast: function (sI18nKey, aArgs) {
            return FeedbackCoordinator.showToast(this, sI18nKey, aArgs || [], FeedbackConstants.SEVERITY.INFO);
        },

        showI18nError: function (sI18nKey, aArgs) {
            return FeedbackCoordinator.showRouteMessage(this, FeedbackConstants.SEVERITY.ERROR, sI18nKey, aArgs || [], sI18nKey);
        },

        applyUseCaseEffects: function (oResult) {
            return FeedbackCoordinator.applyUseCaseResult(this, oResult);
        },

        executeFacadeMethod: function (oFacade, sMethod, mInput, mCtx) {
            var fn = oFacade && oFacade[sMethod];
            if (typeof fn !== "function") {
                return Promise.resolve();
            }
            return Promise.resolve(fn.call(oFacade, mInput || {}, mCtx || {})).then(function (oResult) {
                return this.applyUseCaseEffects(oResult);
            }.bind(this)).catch(function (oRawError) {
                return this.applyUseCaseEffects({
                    ok: false,
                    error: oRawError || {},
                    effects: [],
                    isOk: function () {
                        return false;
                    },
                    getValue: function () {
                        return null;
                    },
                    getError: function () {
                        return oRawError || {};
                    }
                });
            }.bind(this));
        }
    };
});
