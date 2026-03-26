sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/FeedbackDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants"
], function (FeedbackDefaultHandlers, FeedbackConstants) {
    "use strict";

    return {
        showI18nToast: function (sI18nKey, aArgs) {
            return FeedbackDefaultHandlers.handlers.showToast({
                controller: this,
                textKey: sI18nKey,
                args: aArgs || [],
                severity: FeedbackConstants.SEVERITY.INFO,
                fallback: sI18nKey
            });
        },

        showI18nError: function (sI18nKey, aArgs) {
            return FeedbackDefaultHandlers.handlers.showRouteMessage({
                controller: this,
                textKey: sI18nKey,
                args: aArgs || [],
                severity: FeedbackConstants.SEVERITY.ERROR,
                fallback: sI18nKey
            });
        },

        applyUseCaseEffects: function (oResult) {
            return FeedbackDefaultHandlers.handlers.applyUseCaseResult({
                controller: this,
                result: oResult
            });
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
