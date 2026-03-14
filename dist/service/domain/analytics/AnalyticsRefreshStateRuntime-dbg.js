sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/contracts/AnalyticsContracts"
], function (AnalyticsContracts) {
    "use strict";

    function normalizeRefreshState(oRefreshState, fnFormatDateTime) {
        var o = oRefreshState || {};
        return {
            taskKey: String(o.taskKey || AnalyticsContracts.REFRESH.TASK_KEY),
            taskName: String(o.taskName || AnalyticsContracts.REFRESH.TASK_NAME),
            status: String(o.status || AnalyticsContracts.REFRESH.STATUSES.IDLE),
            isRunning: !!o.isRunning,
            requestedAt: fnFormatDateTime(o.requestedAt || ""),
            requestedBy: String(o.requestedBy || ""),
            startedAt: fnFormatDateTime(o.startedAt || ""),
            finishedAt: fnFormatDateTime(o.finishedAt || ""),
            lastSuccessAt: fnFormatDateTime(o.lastSuccessAt || ""),
            lastError: String(o.lastError || ""),
            lastMessage: String(o.lastMessage || ""),
            activeRunId: String(o.activeRunId || "")
        };
    }

    return {
        normalizeRefreshState: normalizeRefreshState
    };
});
