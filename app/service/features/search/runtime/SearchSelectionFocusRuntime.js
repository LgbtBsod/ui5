sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (FocusRuntime, SchedulingRuntime) {
    "use strict";

    function resolveViewDom(oController) {
        return oController && oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
    }

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

    function focusScopedSelector(oController, sSelector) {
        var oViewDom = resolveViewDom(oController);
        if (!oViewDom || !oViewDom.querySelector || !sSelector) {
            return false;
        }
        return focusDomNode(oViewDom.querySelector(sSelector));
    }

    function focusSearchControlDom(oControl) {
        if (!oControl || typeof oControl.getDomRef !== "function") {
            return false;
        }
        return focusDomNode(oControl.getDomRef());
    }

    return {
        focusSearchFilters: function (oController, fnResolveSmartSearchButton) {
            var oTarget = fnResolveSmartSearchButton(oController) || oController.byId("searchSmartFilterBar");
            if (!oTarget) {
                return focusScopedSelector(oController, "[id$='searchSmartFilterBar-btnGo']")
                    || focusScopedSelector(oController, "[id$='searchSmartFilterBar']")
                    || focusScopedSelector(oController, "[id$='searchSmartFilterBar'] input");
            }
            if (FocusRuntime.focusSoon(oTarget)) {
                return true;
            }
            return focusSearchControlDom(oTarget)
                || focusScopedSelector(oController, "[id$='searchSmartFilterBar-btnGo']")
                || focusScopedSelector(oController, "[id$='searchSmartFilterBar']")
                || focusScopedSelector(oController, "[id$='searchSmartFilterBar'] input");
        },

        focusSearchResults: function (oController, fnResolveSearchInnerTable) {
            var oInnerTable = fnResolveSearchInnerTable(oController);
            var aSelectedItems;
            var aItems;
            var oTarget;
            if (!oInnerTable) {
                return focusScopedSelector(oController, "[id$='searchSmartTable']")
                    || focusScopedSelector(oController, ".searchResultsTable");
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
            if (focusSearchControlDom(oTarget)) {
                return true;
            }
            return focusSearchControlDom(oController.byId("searchSmartTable"))
                || focusScopedSelector(oController, ".searchResultsTable .sapMListTblRow")
                || focusScopedSelector(oController, ".searchResultsTable .sapMListTbl");
        },

        focusSearchToolbar: function (oController) {
            var oTarget = oController.byId("backendTopInput")
                || oController.byId("maxRowsInput");
            if (!oTarget) {
                return focusScopedSelector(oController, "[id$='backendTopInput-inner']")
                    || focusScopedSelector(oController, "[id$='backendTopInput']")
                    || focusScopedSelector(oController, "[id$='maxRowsInput-inner']")
                    || focusScopedSelector(oController, "[id$='maxRowsInput']")
                    || focusScopedSelector(oController, ".searchCreateActionBtn");
            }
            if (FocusRuntime.focusSoon(oTarget)) {
                return true;
            }
            if (focusSearchControlDom(oTarget)) {
                return true;
            }
            return focusSearchControlDom(oController.byId("backendTopInput"))
                || focusSearchControlDom(oController.byId("maxRowsInput"))
                || focusScopedSelector(oController, "[id$='backendTopInput-inner']")
                || focusScopedSelector(oController, "[id$='backendTopInput']")
                || focusScopedSelector(oController, "[id$='maxRowsInput-inner']")
                || focusScopedSelector(oController, "[id$='maxRowsInput']")
                || focusScopedSelector(oController, ".searchCreateActionBtn");
        }
    };
});
