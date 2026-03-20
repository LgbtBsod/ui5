sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (FocusRuntime, SchedulingRuntime, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var FILTER_SELECTORS = ["[id$='searchSmartFilterBar-btnGo']", "[id$='searchSmartFilterBar']", "[id$='searchSmartFilterBar'] input", "[role='search']", "input", "button"];
    var RESULTS_SELECTORS = [".searchResultsTable .sapMListTblRow", ".searchResultsTable .sapMListTbl", "[role='row']", "[role='grid']", "[role='table']"];
    var TOOLBAR_SELECTORS = ["[id$='backendTopInput-inner']", "[id$='backendTopInput']", "[id$='maxRowsInput-inner']", "[id$='maxRowsInput']", ".searchCreateActionBtn", "input", "button"];

    function resolveViewDom(oController) {
        var oView = oController && typeof oController.getView === TYPE_FUNCTION && oController.getView();
        return oView && typeof oView[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oView[METHODS.GET_DOM_REF]();
    }

    function focusDomNode(oNode) {
        if (!oNode || typeof oNode[METHODS.FOCUS] !== TYPE_FUNCTION) {
            return false;
        }
        try {
            if (typeof oNode.getAttribute === TYPE_FUNCTION && !oNode.getAttribute("tabindex")) {
                oNode.setAttribute("tabindex", "-1");
            }
        } catch (_error) {
            // Ignore readonly attribute nodes.
        }
        try {
            oNode[METHODS.FOCUS]();
        } catch (_focusError) {
            // Retry on the next macrotask for nodes that are not focusable yet during rerender.
        }
        SchedulingRuntime.restartTimer(0, function () {
            try {
                oNode[METHODS.FOCUS]();
            } catch (_retryFocusError) {
                // Ignore terminal focus failures.
            }
        }, 0);
        return true;
    }

    function focusFirstScopedSelector(oController, aSelectors) {
        var oViewDom = resolveViewDom(oController);
        var oNode;
        if (!oViewDom || typeof oViewDom.querySelector !== TYPE_FUNCTION || !Array.isArray(aSelectors)) {
            return false;
        }
        oNode = aSelectors.reduce(function (oFound, sSelector) {
            return oFound || (sSelector ? oViewDom.querySelector(sSelector) : null);
        }, null);
        return focusDomNode(oNode);
    }

    function focusSearchControlDom(oControl) {
        if (!oControl || typeof oControl[METHODS.GET_DOM_REF] !== TYPE_FUNCTION) {
            return false;
        }
        return focusDomNode(oControl[METHODS.GET_DOM_REF]());
    }

    function focusControl(oControl) {
        if (!oControl) {
            return false;
        }
        if (FocusRuntime.focusSoon(oControl)) {
            return true;
        }
        return focusSearchControlDom(oControl);
    }

    function resolveToolbarControl(oController) {
        return oController.byId("backendTopInput")
            || oController.byId("maxRowsInput")
            || oController.byId("searchCreateButton");
    }

    function resolveResultsTarget(oController, fnResolveSearchInnerTable) {
        var oInnerTable = fnResolveSearchInnerTable(oController);
        var aSelectedItems;
        var aItems;
        if (!oInnerTable) {
            return oController.byId("searchSmartTable");
        }
        aSelectedItems = typeof oInnerTable.getSelectedItems === TYPE_FUNCTION ? (oInnerTable.getSelectedItems() || []) : [];
        if (Array.isArray(aSelectedItems) && aSelectedItems.length) {
            return aSelectedItems[0];
        }
        aItems = typeof oInnerTable.getItems === TYPE_FUNCTION ? (oInnerTable.getItems() || []) : [];
        if (Array.isArray(aItems) && aItems.length) {
            return aItems[0];
        }
        return oInnerTable;
    }

    return {
        focusSearchFilters: function (oController, fnResolveSmartSearchButton) {
            var oTarget = fnResolveSmartSearchButton(oController) || oController.byId("searchSmartFilterBar");
            return focusControl(oTarget)
                || focusFirstScopedSelector(oController, FILTER_SELECTORS);
        },

        focusSearchResults: function (oController, fnResolveSearchInnerTable) {
            var oTarget = resolveResultsTarget(oController, fnResolveSearchInnerTable);
            return focusControl(oTarget)
                || focusFirstScopedSelector(oController, RESULTS_SELECTORS);
        },

        focusSearchToolbar: function (oController) {
            return focusControl(resolveToolbarControl(oController))
                || focusFirstScopedSelector(oController, TOOLBAR_SELECTORS);
        }
    };
});
