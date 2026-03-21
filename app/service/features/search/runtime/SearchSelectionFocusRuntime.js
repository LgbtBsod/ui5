sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (FocusRuntime, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var FILTER_CONTROL_SELECTORS = ["input", "button", "[role='search']", "[role='combobox']"];
    var RESULTS_CONTROL_SELECTORS = ["[role='row']", "[role='grid']", "[role='table']", "tr", ".sapMLIB", ".sapMListTblRow"];
    var TOOLBAR_CONTROL_SELECTORS = ["input", "button", "[role='button']", "[role='combobox']"];
    var FILTER_VIEW_SELECTORS = [
        ".searchFilterSurface input",
        ".searchFilterSurface button",
        "[role='search'] input",
        "[role='search'] button"
    ];
    var RESULTS_VIEW_SELECTORS = [
        ".searchResultsTable [role='row']",
        ".searchResultsTable .sapMLIB",
        ".searchResultsTable .sapMListTblRow",
        ".searchResultsTable [role='grid']",
        ".searchResultsTable [role='table']"
    ];
    var TOOLBAR_VIEW_SELECTORS = [
        ".searchResultsActionRail input",
        ".searchResultsActionRail button",
        ".searchResultsActionRail [role='button']",
        ".searchResultsActionRail [role='combobox']",
        ".searchSettingsRail input",
        ".searchSettingsRail button",
        ".searchSettingsRail [role='button']",
        ".searchSettingsRail [role='combobox']"
    ];

    function focusDomNode(oNode) {
        if (!oNode || typeof oNode[METHODS.FOCUS] !== TYPE_FUNCTION) {
            return false;
        }
        try {
            oNode[METHODS.FOCUS]();
            return true;
        } catch (_focusError) {
            return false;
        }
    }

    function resolveDomRef(oControl) {
        if (!oControl) {
            return null;
        }
        if (typeof oControl[METHODS.GET_FOCUS_DOM_REF] === TYPE_FUNCTION) {
            return oControl[METHODS.GET_FOCUS_DOM_REF]();
        }
        if (typeof oControl[METHODS.GET_DOM_REF] === TYPE_FUNCTION) {
            return oControl[METHODS.GET_DOM_REF]();
        }
        return null;
    }

    function resolveViewDomRef(oController) {
        var oView = oController && typeof oController.getView === TYPE_FUNCTION ? oController.getView() : null;
        return oView && typeof oView[METHODS.GET_DOM_REF] === TYPE_FUNCTION ? oView[METHODS.GET_DOM_REF]() : null;
    }

    function queryFirstFocusable(oRoot, aSelectors) {
        var i;
        var oCandidate;
        if (!oRoot || typeof oRoot.querySelector !== TYPE_FUNCTION || !Array.isArray(aSelectors)) {
            return null;
        }
        for (i = 0; i < aSelectors.length; i += 1) {
            oCandidate = aSelectors[i] ? oRoot.querySelector(aSelectors[i]) : null;
            if (oCandidate) {
                return oCandidate;
            }
        }
        return null;
    }

    function focusControl(oControl, aSelectors) {
        var oDomRef;
        var oFocusable;
        if (!oControl) {
            return false;
        }
        if (typeof oControl[METHODS.FOCUS] === TYPE_FUNCTION) {
            try {
                oControl[METHODS.FOCUS]();
                return true;
            } catch (_focusError) {
                // Fall back to DOM focus below.
            }
        }
        if (FocusRuntime.focusSoon(oControl)) {
            return true;
        }
        oDomRef = resolveDomRef(oControl);
        oFocusable = queryFirstFocusable(oDomRef, aSelectors || []);
        return focusDomNode(oFocusable || oDomRef);
    }

    function focusViewScopedFallback(oController, aSelectors) {
        var oViewDomRef = resolveViewDomRef(oController);
        var oFocusable = queryFirstFocusable(oViewDomRef, aSelectors || []);
        return focusDomNode(oFocusable);
    }

    function resolveToolbarControl(oController) {
        return oController.byId("backendTopInput")
            || oController.byId("maxRowsInput")
            || oController.byId("searchActionRailStack")
            || oController.byId("searchResultsActionRail");
    }

    function resolveFilterControl(oController) {
        return oController.byId("searchSmartFilterBar");
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
        focusSearchFilters: function (oController) {
            return focusControl(resolveFilterControl(oController), FILTER_CONTROL_SELECTORS)
                || focusViewScopedFallback(oController, FILTER_VIEW_SELECTORS);
        },

        focusSearchResults: function (oController, fnResolveSearchInnerTable) {
            var oTarget = resolveResultsTarget(oController, fnResolveSearchInnerTable);
            return focusControl(oTarget, RESULTS_CONTROL_SELECTORS)
                || focusViewScopedFallback(oController, RESULTS_VIEW_SELECTORS);
        },

        focusSearchToolbar: function (oController) {
            return focusControl(resolveToolbarControl(oController), TOOLBAR_CONTROL_SELECTORS)
                || focusViewScopedFallback(oController, TOOLBAR_VIEW_SELECTORS);
        }
    };
});
