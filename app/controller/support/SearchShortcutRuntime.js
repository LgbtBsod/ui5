sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/SearchSelectionRuntime"
], function (SearchSelectionRuntime) {
    "use strict";

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
        var sHash = typeof window !== "undefined" ? String(window.location.hash || "") : "";
        var bSearchRoute = sHash === "" || sHash === "#" || sHash.indexOf("/search") >= 0;
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
        return bSearchRoute;
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
            if (sKey === "n") { return "create"; }
            if (sKey === "c") { return "copy"; }
            if (sKey === "a") { return "selectVisible"; }
            if (sKey === "l") { return "clearSelection"; }
            if (sKey === "s") { return "search"; }
            if (sKey === "e") { return "export"; }
            if (sKey === "f") { return "focusFilters"; }
            return "";
        }
        if (!bAccel && !bShift && bAlt) {
            if (sKey === "1") { return "focusFilters"; }
            if (sKey === "2") { return "focusResults"; }
            if (sKey === "3") { return "focusToolbar"; }
        }
        return "";
    }

    function runShortcutAction(oController, sAction) {
        if (!sAction) {
            return false;
        }
        if (sAction === "create" && typeof oController.onCreate === "function") {
            oController.onCreate();
            return true;
        }
        if (sAction === "copy" && typeof oController.onCopy === "function") {
            oController.onCopy();
            return true;
        }
        if (sAction === "selectVisible" && typeof oController.onSelectVisibleRows === "function") {
            oController.onSelectVisibleRows();
            return true;
        }
        if (sAction === "clearSelection" && typeof oController.onClearSelection === "function") {
            oController.onClearSelection();
            return true;
        }
        if (sAction === "search" && typeof oController.onSmartSearch === "function") {
            oController.onSmartSearch();
            return true;
        }
        if (sAction === "export" && typeof oController.onExportScreen === "function") {
            oController.onExportScreen();
            return true;
        }
        if (sAction === "focusFilters") {
            return SearchSelectionRuntime.focusSearchFilters(oController);
        }
        if (sAction === "focusResults") {
            return SearchSelectionRuntime.focusSearchResults(oController);
        }
        if (sAction === "focusToolbar") {
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

    function bindPowerUserShortcuts(oController) {
        if (typeof document === "undefined" || oController._fnSearchPowerUserShortcut) {
            return;
        }
        oController._fnSearchPowerUserShortcut = function (oEvent) {
            handlePowerUserShortcut(oController, oEvent);
        };
        document.addEventListener("keydown", oController._fnSearchPowerUserShortcut, true);
    }

    function unbindPowerUserShortcuts(oController) {
        if (typeof document === "undefined" || !oController._fnSearchPowerUserShortcut) {
            return;
        }
        document.removeEventListener("keydown", oController._fnSearchPowerUserShortcut, true);
        oController._fnSearchPowerUserShortcut = null;
    }

    return {
        bindPowerUserShortcuts: bindPowerUserShortcuts,
        unbindPowerUserShortcuts: unbindPowerUserShortcuts
    };
});
