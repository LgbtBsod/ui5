sap.ui.define([], function () {
    "use strict";

    function applySelectedChecklist(mArgs) {
        var oChecklist = mArgs && mArgs.checklist ? mArgs.checklist : null;
        var oSelectedModel = mArgs && mArgs.selectedModel;
        var oViewModel = mArgs && mArgs.viewModel;
        if (oSelectedModel && typeof oSelectedModel.setData === "function") {
            oSelectedModel.setData(oChecklist || {});
        }
        if (oViewModel && typeof oViewModel.setProperty === "function") {
            oViewModel.setProperty("/hasSelection", hasChecklistId(oChecklist));
        }
        return oChecklist;
    }

    function hasChecklistId(oChecklist) {
        return !!String((((oChecklist || {}).root || {}).id) || "").trim();
    }

    function shouldProceedAfterUnsavedDecision(sDecision) {
        return sDecision !== "CANCEL";
    }

    function buildNavigationState(sId) {
        return {
            layout: "TwoColumnsMidExpanded",
            route: "detail",
            routeParams: { id: sId }
        };
    }

    function syncSelectionState(mArgs) {
        var oSelectedModel = mArgs && mArgs.selectedModel;
        var oDataModel = mArgs && mArgs.dataModel;
        var oViewModel = mArgs && mArgs.viewModel;

        var sSelectedId = String(((((oSelectedModel && oSelectedModel.getData && oSelectedModel.getData()) || {}).root || {}).id) || "").trim();
        var aVisible = (oDataModel && oDataModel.getProperty && oDataModel.getProperty("/visibleCheckLists")) || [];
        var aAll = (oDataModel && oDataModel.getProperty && oDataModel.getProperty("/checkLists")) || [];
        var aScope = Array.isArray(aVisible) && aVisible.length ? aVisible : aAll;

        var bExists = !!(sSelectedId && aScope.some(function (oRow) {
            return String((((oRow || {}).root || {}).id) || "").trim() === sSelectedId;
        }));

        if (oViewModel && typeof oViewModel.setProperty === "function") {
            oViewModel.setProperty("/hasSelection", bExists);
        }

        return bExists;
    }

    return {
        applySelectedChecklist: applySelectedChecklist,
        shouldProceedAfterUnsavedDecision: shouldProceedAfterUnsavedDecision,
        buildNavigationState: buildNavigationState,
        syncSelectionState: syncSelectionState
    };
});
