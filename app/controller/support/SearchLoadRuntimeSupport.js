sap.ui.define([
    "sap_ui5/controller/support/SearchControllerSupport",
    "sap_ui5/controller/support/ControllerModelWriteSupport"
], function (SearchControllerSupport, ControllerModelWriteSupport) {
    "use strict";

    function setViewProperty(oController, sPath, vValue) {
        ControllerModelWriteSupport.set(oController, "view", sPath, vValue);
    }

    function setStateProperty(oController, sPath, vValue) {
        ControllerModelWriteSupport.set(oController, "state", sPath, vValue);
    }

    function formatSearchDateTime(vDate) {
        if (vDate === null || vDate === undefined || vDate === "") {
            return "-";
        }
        return SearchControllerSupport.formatHumanDateTime(vDate);
    }

    function setSettledViewState(oController, bCanExport, sWorkflowStage) {
        setViewProperty(oController, "/busy", false);
        setViewProperty(oController, "/tableBusy", false);
        setViewProperty(oController, "/canExport", !!bCanExport);
        setViewProperty(oController, "/hasRows", !!bCanExport);
        setViewProperty(oController, "/workflowStage", String(sWorkflowStage || "DISCOVER"));
        setViewProperty(oController, "/lastUpdatedAt", formatSearchDateTime(new Date()));
    }

    function setLoadStatus(oController, mStatus) {
        var oStatus = mStatus || {};
        setStateProperty(oController, "/isLoading", !!oStatus.isLoading);
        setStateProperty(oController, "/isBusy", !!oStatus.isBusy);
        setStateProperty(oController, "/loadError", !!oStatus.loadError);
        setStateProperty(
            oController,
            "/loadErrorMessage",
            oStatus.loadError ? String(oStatus.loadErrorMessage || "Search request failed") : ""
        );
    }

    function markLoading(oController) {
        setViewProperty(oController, "/tableBusy", true);
        setLoadStatus(oController, {
            isLoading: true,
            isBusy: true,
            loadError: false
        });
    }

    function applyLoadError(oController, sErrorMessage) {
        setSettledViewState(oController, false, "REVIEW");
        setLoadStatus(oController, {
            isLoading: false,
            isBusy: false,
            loadError: true,
            loadErrorMessage: sErrorMessage
        });
    }

    function applyLoadSuccess(oController, aRows) {
        var iCount = Array.isArray(aRows) ? aRows.length : 0;
        setSettledViewState(oController, iCount > 0, iCount > 0 ? "ANALYZE" : "DISCOVER");
        setLoadStatus(oController, {
            isLoading: false,
            isBusy: false,
            loadError: false
        });
    }

    return {
        applyLoadError: applyLoadError,
        applyLoadSuccess: applyLoadSuccess,
        markLoading: markLoading,
        setLoadStatus: setLoadStatus,
        setSettledViewState: setSettledViewState,
        setViewProperty: setViewProperty
    };
});
