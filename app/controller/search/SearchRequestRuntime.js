sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchMaxResults",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchRuntimeContracts"
], function (SearchToolbarContracts, SearchMaxResults, SearchViewStateRuntime, ModelStateRuntime, ModelContracts, SearchRuntimeContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;

    function normalizeRequestValue(sNormalizedValue, sFallbackValue) {
        var sSafeFallback = String(sFallbackValue || "").trim();
        return String(sNormalizedValue || "").trim() || sSafeFallback || "100";
    }

    function normalizeOptionalRequestValue(sNormalizedValue) {
        return String(sNormalizedValue || "").trim();
    }

    function syncToolbarRequestInputs(oController) {
        var oBackendTopInput = oController.byId("backendTopInput");
        var oMaxRowsInput = oController.byId("maxRowsInput");
        var sBackendTop = SearchMaxResults.normalizeSearchBackendTopValue(oBackendTopInput && oBackendTopInput.getValue && oBackendTopInput.getValue());
        var sMaxRows = SearchMaxResults.normalizeSearchMaxResultsValue(oMaxRowsInput && oMaxRowsInput.getValue && oMaxRowsInput.getValue());
        sBackendTop = normalizeOptionalRequestValue(sBackendTop);
        sMaxRows = normalizeRequestValue(sMaxRows, ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_MAX_RESULTS, SearchRuntimeContracts.DEFAULTS.SEARCH_VISIBLE_ROWS));
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_BACKEND_TOP, sBackendTop);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_FETCH_LIMIT, sBackendTop);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_MAX_RESULTS, sMaxRows);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.GROWING_PAGE_SIZE, sMaxRows);
        if (oBackendTopInput && typeof oBackendTopInput.setValue === "function") {
            oBackendTopInput.setValue(sBackendTop);
        }
        if (oMaxRowsInput && typeof oMaxRowsInput.setValue === "function") {
            oMaxRowsInput.setValue(sMaxRows);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
    }

    function applyMaxRowsChange(oController, oEvent) {
        var sValue = SearchMaxResults.normalizeSearchMaxResultsValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        sValue = normalizeRequestValue(sValue, ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_MAX_RESULTS, SearchRuntimeContracts.DEFAULTS.SEARCH_VISIBLE_ROWS));
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_MAX_RESULTS, sValue);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.GROWING_PAGE_SIZE, sValue);
        if (oSource && typeof oSource.setValue === "function") {
            oSource.setValue(sValue);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
    }

    function applyBackendTopChange(oController, oEvent) {
        var sValue = SearchMaxResults.normalizeSearchBackendTopValue(oEvent && oEvent.getParameter && oEvent.getParameter("value"));
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        var sCurrentValue;
        sValue = normalizeOptionalRequestValue(sValue);
        sCurrentValue = String(ModelStateRuntime.read(oController, STATE_MODEL, PATHS.SEARCH_BACKEND_TOP, SearchRuntimeContracts.DEFAULTS.SEARCH_BACKEND_TOP) || "").trim();
        if (sCurrentValue === sValue) {
            if (oSource && typeof oSource.setValue === "function") {
                oSource.setValue(sValue);
            }
            return false;
        }
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_BACKEND_TOP, sValue);
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_FETCH_LIMIT, sValue);
        if (oSource && typeof oSource.setValue === "function") {
            oSource.setValue(sValue);
        }
        SearchViewStateRuntime.syncSearchTableRequestWindow(oController);
        return true;
    }

    return {
        syncToolbarRequestInputs: syncToolbarRequestInputs,
        applyMaxRowsChange: applyMaxRowsChange,
        applyBackendTopChange: applyBackendTopChange
    };
});
