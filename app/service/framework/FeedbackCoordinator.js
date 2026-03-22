sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants"
], function (FeedbackConstants) {
    "use strict";

    function feedbackScope() {
        return sap.ui.requireSync("PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes").feedback;
    }

    function runOperation(sOperation, mContext) {
        return feedbackScope().execute(sOperation, mContext || {});
    }

    function runSyncOperation(sOperation, mContext) {
        return feedbackScope().executeSync(sOperation, mContext || {});
    }

    function resolveText(oController, sKey, aArgs, sFallback) {
        return runSyncOperation("resolveText", {
            controller: oController,
            textKey: sKey,
            args: aArgs || [],
            fallback: sFallback
        });
    }

    function applyUseCaseResult(oController, oResult, mOptions) {
        return runOperation("applyUseCaseResult", {
            controller: oController,
            result: oResult || null,
            options: mOptions || {}
        });
    }

    function showGlobalMessage(oController, sSeverity, sTextKey, aArgs, sFallback) {
        return runSyncOperation("showGlobalMessage", {
            controller: oController,
            severity: sSeverity,
            textKey: sTextKey,
            args: aArgs || [],
            fallback: sFallback || sTextKey
        });
    }

    function showRouteMessage(oController, sSeverity, sTextKey, aArgs, sFallback) {
        return runSyncOperation("showRouteMessage", {
            controller: oController,
            severity: sSeverity,
            textKey: sTextKey,
            args: aArgs || [],
            fallback: sFallback || sTextKey
        });
    }

    function showToast(oController, sTextKey, aArgs, sLevel) {
        return runOperation("showToast", {
            controller: oController,
            textKey: sTextKey,
            args: aArgs || [],
            level: sLevel || FeedbackConstants.SEVERITY.INFO
        });
    }

    return {
        resolveText: resolveText,
        applyUseCaseResult: applyUseCaseResult,
        showGlobalMessage: showGlobalMessage,
        showRouteMessage: showRouteMessage,
        showToast: showToast,
        registerBehaviorOverride: function () {
            return feedbackScope().registerBehaviorOverride.apply(null, arguments);
        },
        unregisterBehaviorOverride: function () {
            return feedbackScope().unregisterBehaviorOverride.apply(null, arguments);
        },
        clearBehaviorOverrides: function () {
            return feedbackScope().clearBehaviorOverrides.apply(null, arguments);
        }
    };
});
