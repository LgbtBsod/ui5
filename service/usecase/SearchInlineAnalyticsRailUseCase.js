sap.ui.define([
    "sap_ui5/service/usecase/SearchInlineAnalyticsRefreshOrchestrationUseCase"
], function (SearchInlineAnalyticsRefreshOrchestrationUseCase) {
    "use strict";

    function runTriggerRefresh(mArgs) {
        if (!SearchInlineAnalyticsRefreshOrchestrationUseCase.shouldRefreshForTrigger(mArgs.trigger)) {
            return Promise.resolve({ applied: false, reason: "unsupported_trigger" });
        }

        return SearchInlineAnalyticsRefreshOrchestrationUseCase.runRefreshLifecycle({
            refreshState: mArgs.refreshState,
            viewModel: mArgs.viewModel,
            loadAnalytics: mArgs.loadAnalytics,
            applyPresentation: mArgs.applyPresentation
        });
    }

    function runSimpleRailRefresh(mArgs) {
        mArgs.viewModel.setProperty("/analyticsRailBusy", true);
        return mArgs.loadSimpleAnalytics().then(function (oAnalytics) {
            mArgs.applyPresentation(oAnalytics || {});
            mArgs.viewModel.setProperty("/analyticsError", "");
            return { ok: true, reason: "applied", analytics: oAnalytics || {} };
        }).catch(function (oError) {
            mArgs.viewModel.setProperty("/analyticsError", (oError && oError.message) || "");
            return { ok: false, reason: "load_failed", error: oError || null };
        }).finally(function () {
            mArgs.viewModel.setProperty("/analyticsRailBusy", false);
        });
    }

    return {
        runTriggerRefresh: runTriggerRefresh,
        runSimpleRailRefresh: runSimpleRailRefresh
    };
});
