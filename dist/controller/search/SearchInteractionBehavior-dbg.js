sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchFilterLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchViewBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/SearchCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ReadinessTelemetryContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/OperationSourceContracts"
], function (ControllerViewStateRuntime, SearchFilterLifecycleBehavior, SearchLoadRuntime, SearchViewBehavior, SearchCommandPolicy, ReadinessTelemetryRuntime, ReadinessTelemetryContracts, OperationSourceContracts) {
    "use strict";

    var SEARCH_SOURCES = OperationSourceContracts.SEARCH;

    function withActionBusy(oController, sViewBusyPath, fnAction, fnSyncControlBusy) {
        if (typeof fnSyncControlBusy === "function") {
            fnSyncControlBusy(true);
        }
        return ControllerViewStateRuntime.withFlag(oController, sViewBusyPath, function () {
            return typeof fnAction === "function" ? fnAction() : undefined;
        }).finally(function () {
            if (typeof fnSyncControlBusy === "function") {
                fnSyncControlBusy(false);
            }
        });
    }

    function onSmartSearch(oController) {
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.SEARCH_INTERACTION_READY, {
            action: "smartSearch"
        });
        SearchViewBehavior.beginSearchLoadingFeedback(oController);
        return SearchFilterLifecycleBehavior.onSmartSearch(oController, function (sBusyPath, fnAction) {
            return withActionBusy(oController, sBusyPath, fnAction, function (bBusy) {
                SearchViewBehavior.setSearchActionBusy(oController, bBusy);
            });
        });
    }

    function onRetrySearchLoad(oController) {
        SearchLoadRuntime.markLoading(oController);
        SearchViewBehavior.beginSearchLoadingFeedback(oController);
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
        withActionBusy: withActionBusy
    };
});
