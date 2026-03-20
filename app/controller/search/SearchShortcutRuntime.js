sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/SearchUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EventDelegateRuntime"
], function (SearchSelectionRuntime, SearchUiContracts, EventDelegateRuntime) {
    "use strict";

    var SHORTCUT_ACTIONS = SearchUiContracts.SHORTCUT_ACTIONS;

    function isEditableTarget(oTarget) {
        var sTagName;
        if (!oTarget) {
            return false;
        }
        sTagName = String(oTarget.tagName || "").toUpperCase();
        if (sTagName === "INPUT" || sTagName === "TEXTAREA" || sTagName === "SELECT") {
            return true;
        }
        if (oTarget.isContentEditable) {
            return true;
        }
        return !!(oTarget.closest && oTarget.closest("[contenteditable='true']"));
    }

    function isSearchKeyboardContext(oController, oEvent) {
        var oViewDom = oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        var oTarget = oEvent && oEvent.target;
        var oActive;
        var bViewAttached = !!(oViewDom && typeof document !== "undefined" && document.body && document.body.contains(oViewDom));
        if (!bViewAttached) {
            return false;
        }
        if (oTarget && oViewDom.contains(oTarget)) {
            return true;
        }
        oActive = typeof document !== "undefined" ? document.activeElement : null;
        if (oActive && oViewDom.contains(oActive)) {
            return true;
        }
        return false;
    }

    function resolveShortcutAction(oEvent) {
        var sKey = String((oEvent && oEvent.key) || "").toLowerCase();
        var bAccel = !!(oEvent && (oEvent.ctrlKey || oEvent.metaKey));
        var bShift = !!(oEvent && oEvent.shiftKey);
        var bAlt = !!(oEvent && oEvent.altKey);
        if (!oEvent || oEvent.repeat) {
            return "";
        }
        if ((bAccel && bShift && !bAlt) || (bAccel && bAlt && !bShift)) {
            if (sKey === "n") { return SHORTCUT_ACTIONS.CREATE; }
            if (sKey === "c") { return SHORTCUT_ACTIONS.COPY; }
            if (sKey === "a") { return SHORTCUT_ACTIONS.SELECT_VISIBLE; }
            if (sKey === "l") { return SHORTCUT_ACTIONS.CLEAR_SELECTION; }
            if (sKey === "s") { return SHORTCUT_ACTIONS.SEARCH; }
            if (sKey === "e") { return SHORTCUT_ACTIONS.EXPORT; }
            if (sKey === "f") { return SHORTCUT_ACTIONS.FOCUS_FILTERS; }
            return "";
        }
        if (!bAccel && !bShift && bAlt) {
            if (sKey === "1") { return SHORTCUT_ACTIONS.FOCUS_FILTERS; }
            if (sKey === "2") { return SHORTCUT_ACTIONS.FOCUS_RESULTS; }
            if (sKey === "3") { return SHORTCUT_ACTIONS.FOCUS_TOOLBAR; }
        }
        return "";
    }

    function runShortcutAction(oController, sAction) {
        if (!sAction) {
            return false;
        }
        if (sAction === SHORTCUT_ACTIONS.CREATE && typeof oController.onCreate === "function") {
            oController.onCreate();
            return true;
        }
        if (sAction === SHORTCUT_ACTIONS.COPY && typeof oController.onCopy === "function") {
            oController.onCopy();
            return true;
        }
        if (sAction === SHORTCUT_ACTIONS.SELECT_VISIBLE && typeof oController.onSelectVisibleRows === "function") {
            oController.onSelectVisibleRows();
            return true;
        }
        if (sAction === SHORTCUT_ACTIONS.CLEAR_SELECTION && typeof oController.onClearSelection === "function") {
            oController.onClearSelection();
            return true;
        }
        if (sAction === SHORTCUT_ACTIONS.SEARCH && typeof oController.onSmartSearch === "function") {
            oController.onSmartSearch();
            return true;
        }
        if (sAction === SHORTCUT_ACTIONS.EXPORT && typeof oController.onExportScreen === "function") {
            oController.onExportScreen();
            return true;
        }
        if (sAction === SHORTCUT_ACTIONS.FOCUS_FILTERS) {
            return SearchSelectionRuntime.focusSearchFilters(oController);
        }
        if (sAction === SHORTCUT_ACTIONS.FOCUS_RESULTS) {
            return SearchSelectionRuntime.focusSearchResults(oController);
        }
        if (sAction === SHORTCUT_ACTIONS.FOCUS_TOOLBAR) {
            return SearchSelectionRuntime.focusSearchToolbar(oController);
        }
        return false;
    }

    function handlePowerUserShortcut(oController, oEvent) {
        var sAction;
        if (!isSearchKeyboardContext(oController, oEvent)) {
            return;
        }
        if (isEditableTarget(oEvent.target) && !(oEvent.altKey || ((oEvent.ctrlKey || oEvent.metaKey) && oEvent.shiftKey))) {
            return;
        }
        sAction = resolveShortcutAction(oEvent);
        if (!sAction) {
            return;
        }
        if (runShortcutAction(oController, sAction)) {
            oEvent.preventDefault();
            oEvent.stopPropagation();
        }
    }

    function bindShortcutDomListener(oController) {
        var oViewDom = oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        if (!oViewDom || oController._bSearchShortcutDomBound || !oController._fnSearchPowerUserShortcut) {
            return;
        }
        oViewDom.addEventListener("keydown", oController._fnSearchPowerUserShortcut, true);
        oController._bSearchShortcutDomBound = true;
    }

    function unbindShortcutDomListener(oController) {
        var oViewDom = oController.getView && oController.getView().getDomRef && oController.getView().getDomRef();
        if (!oViewDom || !oController._bSearchShortcutDomBound || !oController._fnSearchPowerUserShortcut) {
            oController._bSearchShortcutDomBound = false;
            return;
        }
        oViewDom.removeEventListener("keydown", oController._fnSearchPowerUserShortcut, true);
        oController._bSearchShortcutDomBound = false;
    }

    function bindPowerUserShortcuts(oController) {
        if (oController._fnSearchPowerUserShortcut) {
            return;
        }
        oController._fnSearchPowerUserShortcut = function (oEvent) {
            handlePowerUserShortcut(oController, oEvent);
        };
        oController._oSearchShortcutDelegate = {
            onAfterRendering: function () {
                bindShortcutDomListener(oController);
            },
            onBeforeRendering: function () {
                unbindShortcutDomListener(oController);
            }
        };
        if (oController.getView && oController.getView()) {
            EventDelegateRuntime.ensure(oController, "_oSearchShortcutDelegate", oController.getView(), oController._oSearchShortcutDelegate, oController);
        }
        bindShortcutDomListener(oController);
    }

    function unbindPowerUserShortcuts(oController) {
        if (!oController._fnSearchPowerUserShortcut) {
            return;
        }
        unbindShortcutDomListener(oController);
        if (oController._oSearchShortcutDelegate && oController.getView && oController.getView()) {
            EventDelegateRuntime.remove(oController, "_oSearchShortcutDelegate", oController.getView());
        }
        oController._fnSearchPowerUserShortcut = null;
        oController._bSearchShortcutDomBound = false;
    }

    return {
        bindPowerUserShortcuts: bindPowerUserShortcuts,
        unbindPowerUserShortcuts: unbindPowerUserShortcuts
    };
});
