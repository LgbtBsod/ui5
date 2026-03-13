sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (FocusRuntime, SchedulingRuntime) {
    "use strict";

    function focusDomNode(oNode) {
        if (!oNode || typeof oNode.focus !== "function") {
            return false;
        }
        try {
            if (typeof oNode.getAttribute === "function" && !oNode.getAttribute("tabindex")) {
                oNode.setAttribute("tabindex", "-1");
            }
        } catch (_error) {
            // Ignore readonly attribute nodes.
        }
        SchedulingRuntime.restartTimer(0, function () {
            oNode.focus();
        }, 0);
        return true;
    }

    function focusDomSelector(sSelector) {
        if (typeof document === "undefined" || !sSelector) {
            return false;
        }
        return focusDomNode(document.querySelector(sSelector));
    }

    return {
        focusSearchFilters: function (oController, fnResolveSmartSearchButton) {
            var oTarget = fnResolveSmartSearchButton(oController) || oController.byId("searchSmartFilterBar");
            if (!oTarget) {
                return focusDomSelector("[id$='searchSmartFilterBar-btnGo']")
                    || focusDomSelector("[id$='searchSmartFilterBar']")
                    || focusDomSelector("[id$='searchSmartFilterBar'] input");
            }
            if (FocusRuntime.focusSoon(oTarget)) {
                return true;
            }
            return focusDomSelector("[id$='searchSmartFilterBar-btnGo']")
                || focusDomSelector("[id$='searchSmartFilterBar']")
                || focusDomSelector("[id$='searchSmartFilterBar'] input");
        },

        focusSearchResults: function (oController, fnResolveSearchInnerTable) {
            var oInnerTable = fnResolveSearchInnerTable(oController);
            var aSelectedItems;
            var aItems;
            var oTarget;
            if (!oInnerTable) {
                return focusDomSelector("[id$='searchSmartTable']")
                    || focusDomSelector(".searchResultsTable");
            }
            aSelectedItems = oInnerTable.getSelectedItems ? (oInnerTable.getSelectedItems() || []) : [];
            if (Array.isArray(aSelectedItems) && aSelectedItems.length) {
                oTarget = aSelectedItems[0];
            }
            if (!oTarget && oInnerTable.getItems) {
                aItems = oInnerTable.getItems() || [];
                if (Array.isArray(aItems) && aItems.length) {
                    oTarget = aItems[0];
                }
            }
            if (!oTarget) {
                oTarget = oInnerTable;
            }
            if (FocusRuntime.focusSoon(oTarget)) {
                return true;
            }
            if (oTarget && typeof oTarget.getDomRef === "function" && focusDomNode(oTarget.getDomRef())) {
                return true;
            }
            return focusDomSelector("[id$='searchSmartTable']")
                || focusDomSelector(".searchResultsTable .sapMListTblRow")
                || focusDomSelector(".searchResultsTable .sapMListTbl");
        },

        focusSearchToolbar: function (oController) {
            var oTarget = oController.byId("backendTopInput")
                || oController.byId("maxRowsInput")
                || oController.byId("smartTableCustomToolbar");
            if (!oTarget) {
                return focusDomSelector("[id$='backendTopInput-inner']")
                    || focusDomSelector("[id$='maxRowsInput-inner']")
                    || focusDomSelector("[id$='smartTableCustomToolbar']");
            }
            if (FocusRuntime.focusSoon(oTarget)) {
                return true;
            }
            if (oTarget && typeof oTarget.getDomRef === "function" && focusDomNode(oTarget.getDomRef())) {
                return true;
            }
            return focusDomSelector("[id$='backendTopInput-inner']")
                || focusDomSelector("[id$='backendTopInput']")
                || focusDomSelector("[id$='maxRowsInput-inner']")
                || focusDomSelector("[id$='maxRowsInput']")
                || focusDomSelector("[id$='smartTableCustomToolbar']")
                || focusDomSelector(".searchCreateActionBtn");
        }
    };
});
