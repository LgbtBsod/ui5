sap.ui.define([], function () {
    "use strict";

    function runSmartTableDataReceivedLifecycle(mArgs) {
        var fnApplyMetrics = mArgs && mArgs.applyMetrics;
        if (typeof fnApplyMetrics !== "function") {
            return { ok: false, reason: "missing_metrics_adapter" };
        }

        fnApplyMetrics((mArgs && mArgs.rawData) || null);

        var fnSyncFilterHint = mArgs && mArgs.syncFilterHint;
        if (typeof fnSyncFilterHint === "function") {
            fnSyncFilterHint();
        }

        var fnRefreshInlineAnalytics = mArgs && mArgs.refreshInlineAnalytics;
        if (typeof fnRefreshInlineAnalytics === "function") {
            fnRefreshInlineAnalytics("SMART_SEARCH");
        }

        return { ok: true, reason: "smart_table_data_received_synced" };
    }

    function runFallbackSearchLifecycle(mArgs) {
        var fnMarkSearchedAndRebind = mArgs && mArgs.markSearchedAndRebind;
        if (typeof fnMarkSearchedAndRebind !== "function") {
            return { ok: false, reason: "missing_rebind_adapter" };
        }

        fnMarkSearchedAndRebind();

        var fnSyncFilterHint = mArgs && mArgs.syncFilterHint;
        if (typeof fnSyncFilterHint === "function") {
            fnSyncFilterHint();
        }

        var fnRefreshInlineAnalytics = mArgs && mArgs.refreshInlineAnalytics;
        if (typeof fnRefreshInlineAnalytics === "function") {
            fnRefreshInlineAnalytics("FALLBACK_SEARCH");
        }

        return { ok: true, reason: "fallback_search_synced" };
    }

    return {
        runSmartTableDataReceivedLifecycle: runSmartTableDataReceivedLifecycle,
        runFallbackSearchLifecycle: runFallbackSearchLifecycle
    };
});
