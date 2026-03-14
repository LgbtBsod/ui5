sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLocationSuggestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/SearchRuntimeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts"
], function (
    ControllerViewStateRuntime,
    ModelStateRuntime,
    SearchCommandPolicy,
    SearchLocationSuggestRuntime,
    SearchRequestRuntime,
    SearchLoadRuntime,
    SearchViewStateRuntime,
    ModelContracts,
    OperationSourceContracts,
    SearchRuntimeContracts,
    SearchToolbarContracts
) {
    "use strict";

    var SEARCH_MODE = SearchRuntimeContracts.SEARCH_MODE;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;

    function onSmartFilterInitialise(oController, fnApplyAnalyticsDrilldownIntent) {
        ControllerViewStateRuntime.set(oController, "/smartFilterReady", true);
        SearchLocationSuggestRuntime.bindLocationSuggest(oController);
        SearchCommandPolicy.buildFilter(oController, { source: SEARCH_SOURCES.SMART_FILTER_INIT });
        fnApplyAnalyticsDrilldownIntent();
    }

    function onSmartFilterChanged(oController) {
        var oSmartFilterBar = oController.byId("searchSmartFilterBar");
        if (!oSmartFilterBar || (typeof oSmartFilterBar.isInitialised === "function" && !oSmartFilterBar.isInitialised())) {
            return;
        }
        SearchLocationSuggestRuntime.bindLocationSuggest(oController);
        SearchCommandPolicy.buildFilter(oController, { source: SEARCH_SOURCES.SMART_FILTER_CHANGED });
    }

    function onMaxRowsChange(oController, oEvent) {
        SearchRequestRuntime.applyMaxRowsChange(oController, oEvent);
    }

    function onBackendTopChange(oController, oEvent) {
        if (SearchRequestRuntime.applyBackendTopChange(oController, oEvent) &&
            ControllerViewStateRuntime.get(oController, "/hasSearched") &&
            ControllerViewStateRuntime.get(oController, "/smartTableReady")) {
            SearchCommandPolicy.rebind(oController, { source: SEARCH_SOURCES.BACKEND_TOP_CHANGE });
        }
    }

    function onSearchModeToggle(oController, oEvent) {
        var bLoose = !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
        ModelStateRuntime.write(oController, STATE_MODEL, PATHS.SEARCH_MODE, bLoose ? SEARCH_MODE.LOOSE : SEARCH_MODE.EXACT);
        SearchCommandPolicy.executeSearch(oController, { intent: SEARCH_SOURCES.SEARCH_MODE_TOGGLE, state: bLoose });
    }

    function onSmartSearch(oController, fnWithActionBusy) {
        if (!SearchViewStateRuntime.isSmartControlsReady(oController)) {
            return Promise.resolve();
        }
        SearchRequestRuntime.syncToolbarRequestInputs(oController);
        SearchLoadRuntime.markLoading(oController);
        return fnWithActionBusy("/searchActionBusy", function () {
            return SearchCommandPolicy.executeSearch(oController, { source: SEARCH_SOURCES.SMART_SEARCH });
        });
    }

    return {
        onBackendTopChange: onBackendTopChange,
        onMaxRowsChange: onMaxRowsChange,
        onSearchModeToggle: onSearchModeToggle,
        onSmartFilterChanged: onSmartFilterChanged,
        onSmartFilterInitialise: onSmartFilterInitialise,
        onSmartSearch: onSmartSearch
    };
});
