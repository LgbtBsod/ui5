sap.ui.define([
    "checklist/app/service/framework/ControllerCtxRuntime",
    "checklist/app/service/framework/FacadeCommandContract",
    "checklist/app/service/framework/FacadeCommandRuntime"
], function (ControllerCtxRuntime, FacadeCommandContract, FacadeCommandRuntime) {
    "use strict";

    function execute(oController, sMethod, mInput) {
        return FacadeCommandRuntime.executeWithContract(
            oController,
            oController && oController._facade,
            sMethod,
            mInput || {},
            ControllerCtxRuntime.buildSearch(oController),
            {
                normalizeMethod: FacadeCommandContract.normalizeSearchMethod,
                normalizePayload: FacadeCommandContract.normalizeSearchPayload
            }
        );
    }

    function buildFilter(oController, mInput) {
        return execute(oController, "buildFilter", mInput);
    }

    function executeSearch(oController, mInput) {
        return execute(oController, "executeSearch", mInput);
    }

    function rebind(oController, mInput) {
        return execute(oController, "rebind", mInput);
    }

    function selectRow(oController, mInput) {
        return execute(oController, "selectRow", mInput);
    }

    function selectionChanged(oController, mInput) {
        return execute(oController, "selectionChanged", mInput);
    }

    function bootstrap(oController, mInput) {
        return execute(oController, "bootstrap", mInput);
    }

    function analytics(oController, mInput) {
        return execute(oController, "analytics", mInput);
    }

    function applyRebindPolicy(oController, mInput) {
        return execute(oController, "applyRebindPolicy", mInput);
    }

    function exportFlow(oController, mInput) {
        return execute(oController, "exportFlow", mInput);
    }

    return {
        buildFilter: buildFilter,
        executeSearch: executeSearch,
        rebind: rebind,
        selectRow: selectRow,
        selectionChanged: selectionChanged,
        bootstrap: bootstrap,
        analytics: analytics,
        applyRebindPolicy: applyRebindPolicy,
        exportFlow: exportFlow
    };
});
