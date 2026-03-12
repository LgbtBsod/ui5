sap.ui.define([], function () {
    "use strict";

    function safeValue(vValue, sFallback) {
        return vValue === undefined || vValue === null || vValue === "" ? (sFallback || "") : vValue;
    }

    function normalizePercent(vValue) {
        var sValue = String(safeValue(vValue, "")).trim();
        return sValue && sValue.indexOf("%") < 0 ? sValue + "%" : sValue;
    }

    function normalizeSourceLabel(sSource, oBundle) {
        var sKey = String(sSource || "ALL").trim().toUpperCase();
        if (sKey === "WEB") {
            return oBundle.getText("analyticsSourceWeb");
        }
        if (sKey === "INTEGRATION") {
            return oBundle.getText("analyticsSourceIntegration");
        }
        return oBundle.getText("analyticsSourceAll");
    }

    function buildSummaryRows(oViewState, oBundle) {
        var oSummary = (oViewState && oViewState.analytics && oViewState.analytics.summary) || {};
        return [
            {
                Scope: oBundle.getText("analyticsReportScopeSummary"),
                Source: normalizeSourceLabel(oViewState && oViewState.selectedSource, oBundle),
                SelectedYear: safeValue(oViewState && oViewState.selectedYear, ""),
                CompareYear: safeValue(oViewState && oViewState.compareYear, ""),
                Metric: oBundle.getText("kpiVisible"),
                Value: safeValue(oSummary.total, 0)
            },
            {
                Scope: oBundle.getText("analyticsReportScopeSummary"),
                Source: normalizeSourceLabel(oViewState && oViewState.selectedSource, oBundle),
                SelectedYear: safeValue(oViewState && oViewState.selectedYear, ""),
                CompareYear: safeValue(oViewState && oViewState.compareYear, ""),
                Metric: oBundle.getText("kpiFailedChecks"),
                Value: safeValue(oSummary.failedChecks, 0)
            },
            {
                Scope: oBundle.getText("analyticsReportScopeSummary"),
                Source: normalizeSourceLabel(oViewState && oViewState.selectedSource, oBundle),
                SelectedYear: safeValue(oViewState && oViewState.selectedYear, ""),
                CompareYear: safeValue(oViewState && oViewState.compareYear, ""),
                Metric: oBundle.getText("kpiFailedBarriers"),
                Value: safeValue(oSummary.failedBarriers, 0)
            },
            {
                Scope: oBundle.getText("analyticsReportScopeSummary"),
                Source: normalizeSourceLabel(oViewState && oViewState.selectedSource, oBundle),
                SelectedYear: safeValue(oViewState && oViewState.selectedYear, ""),
                CompareYear: safeValue(oViewState && oViewState.compareYear, ""),
                Metric: oBundle.getText("analyticsChecksSuccessRate"),
                Value: normalizePercent(oSummary.checksRate)
            },
            {
                Scope: oBundle.getText("analyticsReportScopeSummary"),
                Source: normalizeSourceLabel(oViewState && oViewState.selectedSource, oBundle),
                SelectedYear: safeValue(oViewState && oViewState.selectedYear, ""),
                CompareYear: safeValue(oViewState && oViewState.compareYear, ""),
                Metric: oBundle.getText("analyticsBarriersSuccessRate"),
                Value: normalizePercent(oSummary.barriersRate)
            }
        ];
    }

    function buildMatrixRows(oViewState, oBundle) {
        return ((oViewState && oViewState.analytics && oViewState.analytics.monthlyMatrixRows) || []).map(function (oRow) {
            return {
                Scope: oBundle.getText("analyticsMonthlyMatrixTitle"),
                Source: normalizeSourceLabel(oViewState && oViewState.selectedSource, oBundle),
                SelectedYear: safeValue(oViewState && oViewState.selectedYear, ""),
                CompareYear: safeValue(oViewState && oViewState.compareYear, ""),
                Metric: safeValue(oRow && oRow.metricKey, ""),
                YearLabel: safeValue(oRow && oRow.yearLabel, ""),
                Jan: safeValue(oRow && oRow.jan, ""),
                Feb: safeValue(oRow && oRow.feb, ""),
                Mar: safeValue(oRow && oRow.mar, ""),
                Apr: safeValue(oRow && oRow.apr, ""),
                May: safeValue(oRow && oRow.may, ""),
                Jun: safeValue(oRow && oRow.jun, ""),
                Jul: safeValue(oRow && oRow.jul, ""),
                Aug: safeValue(oRow && oRow.aug, ""),
                Sep: safeValue(oRow && oRow.sep, ""),
                Oct: safeValue(oRow && oRow.oct, ""),
                Nov: safeValue(oRow && oRow.nov, ""),
                Dec: safeValue(oRow && oRow.dec, "")
            };
        });
    }

    function buildBuilderRows(oViewState, oBundle) {
        return ((oViewState && oViewState.builderChartRows) || []).map(function (oRow) {
            return {
                Scope: oBundle.getText("analyticsBuilderTab"),
                Source: normalizeSourceLabel(oViewState && oViewState.selectedSource, oBundle),
                SelectedYear: safeValue(oViewState && oViewState.selectedYear, ""),
                CompareYear: safeValue(oViewState && oViewState.compareYear, ""),
                Dimension: safeValue(oRow && oRow.label, ""),
                Metric: safeValue(oViewState && oViewState.builderMetric, ""),
                Value: safeValue(oRow && oRow.value, 0)
            };
        });
    }

    function buildRows(oViewState, oBundle) {
        return []
            .concat(buildSummaryRows(oViewState, oBundle))
            .concat(buildMatrixRows(oViewState, oBundle))
            .concat(buildBuilderRows(oViewState, oBundle));
    }

    return {
        buildRows: buildRows
    };
});
