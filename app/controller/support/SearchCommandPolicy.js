sap.ui.define([], function () {
    "use strict";

    function buildFilter(oController, mInput) {
        return oController._execute("buildFilter", mInput || {});
    }

    function executeSearch(oController, mInput) {
        return oController._execute("executeSearch", mInput || {});
    }

    function rebind(oController, mInput) {
        return oController._execute("rebind", mInput || {});
    }

    function selectRow(oController, mInput) {
        return oController._execute("selectRow", mInput || {});
    }

    function selectionChanged(oController, mInput) {
        return oController._execute("selectionChanged", mInput || {});
    }

    function bootstrap(oController, mInput) {
        return oController._execute("bootstrap", mInput || {});
    }

    function analytics(oController, mInput) {
        return oController._execute("analytics", mInput || {});
    }

    function applyRebindPolicy(oController, mInput) {
        return oController._execute("applyRebindPolicy", mInput || {});
    }

    function exportFlow(oController, mInput) {
        return oController._execute("exportFlow", mInput || {});
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
