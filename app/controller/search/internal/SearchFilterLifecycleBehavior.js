sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchLocationSuggestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchToolbarContracts"
], function (
    ControllerViewStateRuntime,
    ModelStateRuntime,
    StatePaths,
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
    var SEARCH_SEGMENTS = SearchRuntimeContracts.SEARCH_SEGMENTS;
    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = SearchToolbarContracts.PATHS;
    var SEARCH_CHECKS_FAIL_SEGMENT = StatePaths.SEARCH_CHECKS_FAIL_SEGMENT;
    var SEARCH_BARRIERS_FAIL_SEGMENT = StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT;
    var CLEAR_RESET_DELAY_MS = 0;

    function deferCustomSegmentReset(oController) {
        if (typeof setTimeout === "function") {
            setTimeout(function () {
                resetCustomSegments(oController);
            }, CLEAR_RESET_DELAY_MS);
            return;
        }
        resetCustomSegments(oController);
    }

    function bindClearButtonPress(oController) {
        var oClearButton;
        var sClearButtonId;
        if (!oController || oController._bSearchClearButtonPressBound) {
            return;
        }
        sClearButtonId = oController.getView() && oController.getView().createId("searchSmartFilterBar-btnClear");
        oClearButton = sClearButtonId && sap.ui.getCore().byId(sClearButtonId);
        if (!oClearButton || typeof oClearButton.attachPress !== "function") {
            return;
        }
        oClearButton.attachPress(function () {
            deferCustomSegmentReset(oController);
        });
        if (typeof oClearButton.attachBrowserEvent === "function") {
            oClearButton.attachBrowserEvent("click", function () {
                deferCustomSegmentReset(oController);
            });
        }
        oController._bSearchClearButtonPressBound = true;
    }

    function resetCustomSegments(oController) {
        var oChecksSegment;
        var oBarriersSegment;
        ModelStateRuntime.setMany(oController, STATE_MODEL, {
            [SEARCH_CHECKS_FAIL_SEGMENT]: SEARCH_SEGMENTS.ALL,
            [SEARCH_BARRIERS_FAIL_SEGMENT]: SEARCH_SEGMENTS.ALL
        });
        oChecksSegment = oController.byId("FailChecksSegmentControl");
        oBarriersSegment = oController.byId("FailBarriersSegmentControl");
        if (oChecksSegment && typeof oChecksSegment.setSelectedKey === "function") {
            oChecksSegment.setSelectedKey(SEARCH_SEGMENTS.ALL);
        }
        if (oBarriersSegment && typeof oBarriersSegment.setSelectedKey === "function") {
            oBarriersSegment.setSelectedKey(SEARCH_SEGMENTS.ALL);
        }
    }

    function bindClearHandler(oController) {
        var oSmartFilterBar;
        if (!oController || oController._bSearchClearHandlerBound) {
            return;
        }
        oSmartFilterBar = oController.byId("searchSmartFilterBar");
        if (!oSmartFilterBar || typeof oSmartFilterBar.attachClear !== "function") {
            return;
        }
        oSmartFilterBar.attachClear(function () {
            deferCustomSegmentReset(oController);
        });
        bindClearButtonPress(oController);
        if (!oController._bSearchClearDelegateBound) {
            oSmartFilterBar.addEventDelegate({
                onAfterRendering: function () {
                    bindClearButtonPress(oController);
                }
            });
            oController._bSearchClearDelegateBound = true;
        }
        if (typeof setTimeout === "function") {
            setTimeout(function () {
                bindClearButtonPress(oController);
            });
        }
        oController._bSearchClearHandlerBound = true;
    }

    function onSmartFilterInitialise(oController, fnApplyAnalyticsDrilldownIntent) {
        ControllerViewStateRuntime.set(oController, "/smartFilterReady", true);
        SearchLocationSuggestRuntime.bindLocationSuggest(oController);
        bindClearHandler(oController);
        deferCustomSegmentReset(oController);
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

    function onSmartFilterClear(oController) {
        deferCustomSegmentReset(oController);
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
        onSmartFilterClear: onSmartFilterClear,
        onSmartFilterChanged: onSmartFilterChanged,
        onSmartFilterInitialise: onSmartFilterInitialise,
        onSmartSearch: onSmartSearch
    };
});
