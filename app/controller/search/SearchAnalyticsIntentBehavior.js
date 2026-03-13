sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy"
], function (ControllerViewStateRuntime, ModelStateRuntime, SearchCommandPolicy) {
    "use strict";

    function readAnalyticsDrilldownIntent(oController, sStateModel, sIntentPath) {
        return ModelStateRuntime.read(oController, sStateModel, sIntentPath, null);
    }

    function clearAnalyticsDrilldownIntent(oController, sStateModel, sIntentPath) {
        ModelStateRuntime.write(oController, sStateModel, sIntentPath, null);
    }

    function applyAnalyticsDrilldownIntent(oController, mOptions) {
        var oIntent = readAnalyticsDrilldownIntent(oController, mOptions.stateModel, mOptions.intentPath) || {};
        var sFilterKey = String(oIntent.filterKey || "").trim();
        var sFilterValue = String(oIntent.filterValue || "").trim();
        var oSmartFilterBar = oController.byId("searchSmartFilterBar");
        var oControl;
        var mFilterData;

        if (!sFilterKey || !sFilterValue || !oSmartFilterBar) {
            return false;
        }
        if (typeof oSmartFilterBar.isInitialised === "function" && !oSmartFilterBar.isInitialised()) {
            return false;
        }
        oControl = typeof oSmartFilterBar.getControlByKey === "function" ? oSmartFilterBar.getControlByKey(sFilterKey) : null;
        if (oControl && typeof oControl.setSelectedKey === "function") {
            oControl.setSelectedKey(sFilterValue);
        }
        if (oControl && typeof oControl.setValue === "function") {
            oControl.setValue(sFilterValue);
        }
        if (oControl && typeof oControl.setTokens === "function") {
            oControl.setTokens([]);
        }
        if (typeof oSmartFilterBar.getFilterData === "function" && typeof oSmartFilterBar.setFilterData === "function") {
            mFilterData = Object.assign({}, oSmartFilterBar.getFilterData() || {});
            mFilterData[sFilterKey] = sFilterValue;
            oSmartFilterBar.setFilterData(mFilterData, true);
        }
        clearAnalyticsDrilldownIntent(oController, mOptions.stateModel, mOptions.intentPath);
        SearchCommandPolicy.buildFilter(oController, { source: mOptions.source });
        if (ControllerViewStateRuntime.get(oController, mOptions.smartTableReadyPath)) {
            SearchCommandPolicy.rebind(oController, { source: mOptions.source });
        }
        return true;
    }

    return {
        applyAnalyticsDrilldownIntent: applyAnalyticsDrilldownIntent
    };
});
