sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/FeedbackDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/FeedbackOverrideHandlers"
], function (BehaviorResolver, FeedbackDefaultHandlers, FeedbackOverrideHandlers) {
    "use strict";

    function runOperation(sOperation, mContext) {
        FeedbackDefaultHandlers.ensureRegistered();
        FeedbackOverrideHandlers.ensureRegistered();
        return BehaviorResolver.execute("feedback", sOperation, mContext || {}, FeedbackDefaultHandlers.handlers);
    }

    function runSyncOperation(sOperation, mContext) {
        FeedbackDefaultHandlers.ensureRegistered();
        FeedbackOverrideHandlers.ensureRegistered();
        return BehaviorResolver.executeSync("feedback", sOperation, mContext || {}, FeedbackDefaultHandlers.handlers);
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
            level: sLevel || "info"
        });
    }

    return {
        resolveText: resolveText,
        applyUseCaseResult: applyUseCaseResult,
        showGlobalMessage: showGlobalMessage,
        showRouteMessage: showRouteMessage,
        showToast: showToast,
        registerBehaviorOverride: FeedbackOverrideHandlers.register,
        unregisterBehaviorOverride: FeedbackOverrideHandlers.unregister,
        clearBehaviorOverrides: FeedbackOverrideHandlers.clear
    };
});
