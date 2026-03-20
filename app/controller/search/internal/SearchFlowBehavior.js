sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchFilterLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchViewLoadBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerActionBusyRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ReadinessTelemetryContracts"
], function (SearchCommandPolicy, SearchFilterLifecycleBehavior, SearchViewLoadBehavior, SearchSelectionRuntime, ControlStyleRuntime, SearchLoadRuntime, ControllerActionBusyRuntime, ReadinessTelemetryRuntime, OperationSourceContracts, ReadinessTelemetryContracts) {
    "use strict";

    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;

    function setSearchActionBusy(oController, bBusy) {
        var oSearchButton = SearchSelectionRuntime.resolveSmartSearchButton(oController);
        if (!oSearchButton) {
            return;
        }
        ControlStyleRuntime.enable(oSearchButton, "searchGoActionBtn");
        if (typeof oSearchButton.setBusy === "function") {
            oSearchButton.setBusy(!!bBusy);
            oSearchButton.setBusyIndicatorDelay(0);
        }
        if (typeof oSearchButton.setEnabled === "function") {
            oSearchButton.setEnabled(!bBusy);
        }
    }

    function onSmartSearch(oController) {
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.SEARCH_INTERACTION_READY, {
            action: "smartSearch"
        });
        SearchViewLoadBehavior.beginSearchLoadingFeedback(oController);
        return SearchFilterLifecycleBehavior.onSmartSearch(oController, function (sBusyPath, fnAction) {
            return ControllerActionBusyRuntime.withActionBusy(oController, sBusyPath, fnAction, function (bBusy) {
                setSearchActionBusy(oController, bBusy);
            });
        });
    }

    function onRetrySearchLoad(oController) {
        SearchLoadRuntime.markLoading(oController);
        SearchViewLoadBehavior.beginSearchLoadingFeedback(oController);
        return SearchCommandPolicy.rebind(oController, { source: SEARCH_SOURCES.SEARCH_RETRY }).finally(function () {
            SearchLoadRuntime.setLoadStatus(oController, { isLoading: false, isBusy: false, loadError: false });
        }).catch(function (oError) {
            SearchLoadRuntime.applyLoadError(oController, String((oError && oError.message) || "Unable to load search results."));
            return Promise.reject(oError);
        });
    }

    return {
        onRetrySearchLoad: onRetrySearchLoad,
        onSmartSearch: onSmartSearch,
        withActionBusy: ControllerActionBusyRuntime.withActionBusy
    };
});
