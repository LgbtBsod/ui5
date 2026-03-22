sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsMonthRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds"
], function (ControllerViewStateRuntime, ModelStateRuntime, SearchCommandPolicy, AnalyticsMonthRuntime, JsRuntime, ModelContracts, StatePaths, UiControlIds) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function normalizeText(vValue) {
        return String(vValue || "").trim();
    }

    function buildSelectionValue(sValue) {
        var sText = normalizeText(sValue);
        if (!sText) {
            return null;
        }
        return {
            items: [{
                key: sText,
                text: sText
            }]
        };
    }

    function resolveYearNumber(oIntent) {
        var iYear = Number(normalizeText(oIntent && oIntent.selectedYear));
        return iYear > 0 ? iYear : 0;
    }

    function resolveMonthIndex(sMonthLabel) {
        return AnalyticsMonthRuntime.resolveMonthIndex(sMonthLabel);
    }

    function toIsoDatePart(iYear, iMonthIndex, iDay) {
        var sMonth = String(iMonthIndex + 1).padStart(2, "0");
        var sDay = String(iDay).padStart(2, "0");
        return String(iYear) + "-" + sMonth + "-" + sDay;
    }

    function buildDateRangeValue(oIntent) {
        var iYear = resolveYearNumber(oIntent);
        var sMonthLabel = normalizeText(oIntent && oIntent.extras && oIntent.extras.monthLabel);
        var iMonthIndex = resolveMonthIndex(sMonthLabel);
        var oStartDate;
        var oEndDate;

        if (!iYear) {
            return null;
        }
        if (Number.isInteger(iMonthIndex)) {
            oStartDate = new Date(iYear, iMonthIndex, 1);
            oEndDate = new Date(iYear, iMonthIndex + 1, 0);
        } else {
            oStartDate = new Date(iYear, 0, 1);
            oEndDate = new Date(iYear, 11, 31);
        }
        return {
            ranges: [{
                exclude: false,
                keyField: "DateCheck",
                operation: "BT",
                value1: toIsoDatePart(oStartDate.getFullYear(), oStartDate.getMonth(), oStartDate.getDate()),
                value2: toIsoDatePart(oEndDate.getFullYear(), oEndDate.getMonth(), oEndDate.getDate())
            }]
        };
    }

    function applySegmentState(oController, oIntent) {
        var sMetric = normalizeText(oIntent && oIntent.extras && oIntent.extras.metric).toUpperCase();

        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.SEARCH_CHECKS_FAIL_SEGMENT, "ALL");
        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT, "ALL");

        if (sMetric === "FAILED_CHECKS" || sMetric === "FAILED_CHECKLISTS") {
            ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.SEARCH_CHECKS_FAIL_SEGMENT, "FAILED");
        }
        if (sMetric === "FAILED_BARRIERS" || sMetric === "FAILED_BARRIER_CHECKLISTS") {
            ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT, "FAILED");
        }
    }

    function mergeIntentFilterData(mFilterData, oIntent, sFilterKey, sFilterValue) {
        var sSource = normalizeText(oIntent && oIntent.analyticsSource).toUpperCase();
        var oDateRange = buildDateRangeValue(oIntent);

        if (sSource && sSource !== "ALL") {
            mFilterData.SourceKey = buildSelectionValue(sSource);
        } else if (Object.prototype.hasOwnProperty.call(mFilterData, "SourceKey")) {
            delete mFilterData.SourceKey;
        }

        if (oDateRange) {
            mFilterData.DateCheck = oDateRange;
        }

        if (!sFilterKey || !sFilterValue) {
            return;
        }
        if (sFilterKey === "DateCheck") {
            if (oDateRange) {
                mFilterData.DateCheck = oDateRange;
            }
            return;
        }
        mFilterData[sFilterKey] = buildSelectionValue(sFilterValue) || sFilterValue;
    }

    function readAnalyticsDrilldownIntent(oController, sStateModel, sIntentPath) {
        return ModelStateRuntime.read(oController, sStateModel, sIntentPath, null);
    }

    function clearAnalyticsDrilldownIntent(oController, sStateModel, sIntentPath) {
        ModelStateRuntime.write(oController, sStateModel, sIntentPath, null);
    }

    function applyAnalyticsDrilldownIntent(oController, mOptions) {
        var oIntent = readAnalyticsDrilldownIntent(oController, mOptions.stateModel, mOptions.intentPath) || {};
        var oCommandPolicy = (mOptions && mOptions.commandPolicy) || SearchCommandPolicy;
        var sFilterKey = normalizeText(oIntent.filterKey);
        var sFilterValue = normalizeText(oIntent.filterValue);
        var oSmartFilterBar = oController.byId(UiControlIds.SEARCH.SMART_FILTER_BAR);
        var oControl;
        var mFilterData;
        var sSource = normalizeText(oIntent.analyticsSource).toUpperCase();
        var bHasScope = !!(sFilterKey && sFilterValue) || !!buildDateRangeValue(oIntent) || (sSource && sSource !== "ALL");

        if (!bHasScope || !oSmartFilterBar) {
            return false;
        }
        if (typeof oSmartFilterBar.isInitialised === TYPE_FUNCTION && !oSmartFilterBar.isInitialised()) {
            return false;
        }
        oControl = sFilterKey && typeof oSmartFilterBar.getControlByKey === TYPE_FUNCTION ? oSmartFilterBar.getControlByKey(sFilterKey) : null;
        if (oControl && typeof oControl[METHODS.SET_SELECTED_KEY] === TYPE_FUNCTION) {
            oControl[METHODS.SET_SELECTED_KEY](sFilterValue);
        }
        if (oControl && typeof oControl[METHODS.SET_VALUE] === TYPE_FUNCTION) {
            oControl[METHODS.SET_VALUE](sFilterValue);
        }
        if (oControl && typeof oControl[METHODS.SET_TOKENS] === TYPE_FUNCTION) {
            oControl[METHODS.SET_TOKENS]([]);
        }
        if (typeof oSmartFilterBar.getFilterData === TYPE_FUNCTION && typeof oSmartFilterBar.setFilterData === TYPE_FUNCTION) {
            mFilterData = Object.assign({}, oSmartFilterBar.getFilterData() || {});
            mergeIntentFilterData(mFilterData, oIntent, sFilterKey, sFilterValue);
            oSmartFilterBar.setFilterData(mFilterData, true);
        }
        applySegmentState(oController, oIntent);
        clearAnalyticsDrilldownIntent(oController, mOptions.stateModel, mOptions.intentPath);
        if (oCommandPolicy && typeof oCommandPolicy.buildFilter === TYPE_FUNCTION) {
            oCommandPolicy.buildFilter(oController, { source: mOptions.source });
        }
        if (ControllerViewStateRuntime.get(oController, mOptions.smartTableReadyPath) && oCommandPolicy && typeof oCommandPolicy.rebind === TYPE_FUNCTION) {
            oCommandPolicy.rebind(oController, { source: mOptions.source });
        }
        return true;
    }

    return {
        applyAnalyticsDrilldownIntent: applyAnalyticsDrilldownIntent
    };
});
