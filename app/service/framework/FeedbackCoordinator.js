sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/FeedbackDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/FeedbackBehaviorHelpers"
], function (FeedbackDefaultHandlers, FeedbackBehaviorHelpers) {
    "use strict";

    function buildContext(oController, mInput) {
        return Object.assign({
            controller: oController
        }, mInput || {});
    }

    function showToast(oController, sTextKey, aArgs, sSeverity, sFallback) {
        return FeedbackDefaultHandlers.showToast(buildContext(oController, {
            textKey: sTextKey,
            args: aArgs || [],
            severity: sSeverity,
            fallback: sFallback || sTextKey
        }));
    }

    function showRouteMessage(oController, sSeverity, sTextKey, aArgs, sFallback) {
        return FeedbackDefaultHandlers.showRouteMessage(buildContext(oController, {
            textKey: sTextKey,
            args: aArgs || [],
            severity: sSeverity,
            fallback: sFallback || sTextKey
        }));
    }

    function applyUseCaseResult(oController, oResult) {
        return FeedbackDefaultHandlers.applyUseCaseResult(buildContext(oController, {
            result: oResult
        }));
    }

    function resolveText(oController, sTextKey, aArgs, sFallback) {
        return FeedbackBehaviorHelpers.resolveText(oController, sTextKey, aArgs || [], sFallback || sTextKey);
    }

    return Object.freeze({
        applyUseCaseResult: applyUseCaseResult,
        resolveText: resolveText,
        showRouteMessage: showRouteMessage,
        showToast: showToast
    });
});
