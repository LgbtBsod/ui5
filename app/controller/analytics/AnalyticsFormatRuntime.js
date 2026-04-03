sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (
    AnalyticsContracts,
    UiSemanticConstants
) {
    "use strict";

    function coerceText(vValue) {
        return String(vValue || "").trim();
    }

    function formatMatrixMetricLabel(sMetricKey, fnGetText) {
        var sKey = coerceText(sMetricKey).toUpperCase();

        if (sKey === AnalyticsContracts.METRICS.TOTAL) {
            return fnGetText("analyticsMetricTotal", []);
        }
        if (sKey === AnalyticsContracts.METRICS.FAILED_CHECKS) {
            return fnGetText("analyticsMetricFailedChecks", []);
        }
        if (sKey === AnalyticsContracts.METRICS.FAILED_BARRIERS) {
            return fnGetText("analyticsMetricFailedBarriers", []);
        }
        if (sKey === AnalyticsContracts.METRICS.FAILED_CHECKLISTS) {
            return fnGetText("analyticsMetricFailedChecklistCount", []);
        }
        if (sKey === AnalyticsContracts.METRICS.FAILED_BARRIER_CHECKLISTS) {
            return fnGetText("analyticsMetricFailedBarrierChecklistCount", []);
        }
        return sMetricKey;
    }

    function formatSourceContext(sSelectedSource, fnGetText) {
        var sSourceKey = coerceText(sSelectedSource).toUpperCase();
        var sResolvedSourceText = fnGetText("analyticsSourceAll", []);

        if (sSourceKey === AnalyticsContracts.SOURCES.WEB) {
            sResolvedSourceText = fnGetText("analyticsSourceWeb", []);
        } else if (sSourceKey === AnalyticsContracts.SOURCES.INTEGRATION) {
            sResolvedSourceText = fnGetText("analyticsSourceIntegration", []);
        }

        return fnGetText("analyticsSourceFilterLabel", []) + ": " + sResolvedSourceText;
    }

    function formatSourceText(sSelectedSource, fnGetText) {
        var sSourceKey = coerceText(sSelectedSource).toUpperCase();

        if (sSourceKey === AnalyticsContracts.SOURCES.WEB) {
            return fnGetText("analyticsSourceWeb", []);
        }
        if (sSourceKey === AnalyticsContracts.SOURCES.INTEGRATION) {
            return fnGetText("analyticsSourceIntegration", []);
        }
        if (sSourceKey === AnalyticsContracts.SOURCES.ALL) {
            return fnGetText("analyticsSourceAll", []);
        }
        return coerceText(sSelectedSource) || "-";
    }

    function formatMonthText(sMonthValue, fnGetText) {
        var sKey = coerceText(sMonthValue).toUpperCase();
        var mMonthTextKeys = {
            JAN: "analyticsMonthShortJan",
            FEB: "analyticsMonthShortFeb",
            MAR: "analyticsMonthShortMar",
            APR: "analyticsMonthShortApr",
            MAY: "analyticsMonthShortMay",
            JUN: "analyticsMonthShortJun",
            JUL: "analyticsMonthShortJul",
            AUG: "analyticsMonthShortAug",
            SEP: "analyticsMonthShortSep",
            OCT: "analyticsMonthShortOct",
            NOV: "analyticsMonthShortNov",
            DEC: "analyticsMonthShortDec",
            "ЯНВ": "analyticsMonthShortJan",
            "ФЕВ": "analyticsMonthShortFeb",
            "МАР": "analyticsMonthShortMar",
            "АПР": "analyticsMonthShortApr",
            "МАЙ": "analyticsMonthShortMay",
            "ИЮН": "analyticsMonthShortJun",
            "ИЮЛ": "analyticsMonthShortJul",
            "АВГ": "analyticsMonthShortAug",
            "СЕН": "analyticsMonthShortSep",
            "ОКТ": "analyticsMonthShortOct",
            "НОЯ": "analyticsMonthShortNov",
            "ДЕК": "analyticsMonthShortDec"
        };
        var sTextKey = mMonthTextKeys[sKey];

        return sTextKey ? fnGetText(sTextKey, []) : coerceText(sMonthValue);
    }

    function localizeChartLabel(sLabel, fnGetText) {
        var sNormalized = coerceText(sLabel).toUpperCase();
        var sSourceText = formatSourceText(sNormalized, fnGetText);
        var sMonthText = formatMonthText(sNormalized, fnGetText);

        if (sNormalized === AnalyticsContracts.SOURCES.ALL
            || sNormalized === AnalyticsContracts.SOURCES.WEB
            || sNormalized === AnalyticsContracts.SOURCES.INTEGRATION) {
            return sSourceText;
        }
        if (sMonthText && sMonthText !== sLabel) {
            return sMonthText;
        }
        return coerceText(sLabel);
    }

    function formatRefreshStatusState(oRefreshState) {
        var sNormalizedStatus = coerceText(oRefreshState && oRefreshState.status).toUpperCase();
        var bIsRunning = !!(oRefreshState && oRefreshState.isRunning);

        if (sNormalizedStatus === AnalyticsContracts.REFRESH_STATUS.ERROR) {
            return UiSemanticConstants.OBJECT_STATUS_STATE.ERROR;
        }
        if (bIsRunning) {
            return UiSemanticConstants.OBJECT_STATUS_STATE.WARNING;
        }
        if (sNormalizedStatus === AnalyticsContracts.REFRESH_STATUS.SUCCESS || sNormalizedStatus === AnalyticsContracts.REFRESH_STATUS.READY) {
            return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
        }
        return UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION;
    }

    function formatRefreshStatusText(oRefreshState, fnGetText) {
        var sStatus = coerceText(oRefreshState && oRefreshState.status).toUpperCase();
        var sMessage = coerceText(oRefreshState && (oRefreshState.lastMessage || oRefreshState.lastError));

        if (!sStatus) {
            return fnGetText("analyticsRefreshIdle", []);
        }
        if (sStatus === AnalyticsContracts.REFRESH_STATUS.REQUESTED) {
            if (sMessage) {
                return sMessage;
            }
            return fnGetText("analyticsRefreshQueued", []);
        }
        if (sStatus === AnalyticsContracts.REFRESH_STATUS.RUNNING) {
            return sMessage
                ? fnGetText("analyticsRefreshRunning", []) + ": " + sMessage
                : fnGetText("analyticsRefreshRunning", []);
        }
        if (sStatus === AnalyticsContracts.REFRESH_STATUS.SUCCESS || sStatus === AnalyticsContracts.REFRESH_STATUS.READY) {
            return fnGetText("analyticsRefreshIdle", []);
        }
        if (sMessage) {
            return sMessage;
        }
        return sStatus;
    }

    function formatRefreshEnabled(bRefreshBusy, oRefreshState) {
        var sStatus = coerceText(oRefreshState && oRefreshState.status).toUpperCase();
        var bIsRunning = !!(oRefreshState && oRefreshState.isRunning);

        return !bRefreshBusy && !bIsRunning && sStatus !== AnalyticsContracts.REFRESH_STATUS.REQUESTED;
    }

    function formatRateState(sRate) {
        var fRate = parseFloat(String(sRate || "0").replace(/[^0-9.]/g, ""));

        if (!Number.isFinite(fRate)) {
            return UiSemanticConstants.OBJECT_STATUS_STATE.NONE;
        }
        if (fRate >= 80) {
            return UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS;
        }
        if (fRate >= 50) {
            return UiSemanticConstants.OBJECT_STATUS_STATE.WARNING;
        }
        return UiSemanticConstants.OBJECT_STATUS_STATE.ERROR;
    }

    function formatRefreshMessageType(oRefreshState) {
        return coerceText(oRefreshState && oRefreshState.lastError)
            ? AnalyticsContracts.REFRESH_MESSAGE_TYPE.ERROR
            : AnalyticsContracts.REFRESH_MESSAGE_TYPE.ACTIVE;
    }

    function formatRefreshMessageVisible(oRefreshState) {
        var sStatus = coerceText(oRefreshState && oRefreshState.status).toUpperCase();

        return sStatus === AnalyticsContracts.REFRESH_STATUS.REQUESTED ||
            !!(oRefreshState && oRefreshState.isRunning) ||
            !!coerceText(oRefreshState && oRefreshState.lastError);
    }

    return {
        formatMatrixMetricLabel: formatMatrixMetricLabel,
        formatRateState: formatRateState,
        formatRefreshEnabled: formatRefreshEnabled,
        formatRefreshMessageType: formatRefreshMessageType,
        formatRefreshMessageVisible: formatRefreshMessageVisible,
        formatRefreshStatusState: formatRefreshStatusState,
        formatRefreshStatusText: formatRefreshStatusText,
        formatSourceContext: formatSourceContext,
        formatSourceText: formatSourceText,
        formatMonthText: formatMonthText,
        localizeChartLabel: localizeChartLabel
    };
});
