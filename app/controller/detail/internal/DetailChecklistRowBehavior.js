sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/BindingContextReader",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBehaviorRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/OperationSourceContracts"
], function (BindingContextReader, DetailCommandPolicy, DetailInfoCardLayoutRuntime, DetailRowBehaviorRuntime, OperationSourceContracts) {
    "use strict";

    var DETAIL_SOURCES = OperationSourceContracts.DETAIL;

    function readSourceFlag(oSource, sKey) {
        var oCursor = oSource;
        var vValue;
        while (oCursor) {
            if (typeof oCursor.data === "function") {
                vValue = oCursor.data(sKey);
                if (typeof vValue !== "undefined" && vValue !== null && vValue !== "") {
                    return String(vValue).trim();
                }
            }
            oCursor = oCursor.getParent && oCursor.getParent();
        }
        return "";
    }

    function resolveRowKind(oEvent) {
        var sKind = readSourceFlag(oEvent && oEvent.getSource && oEvent.getSource(), "rowKind")
            || readSourceFlag(oEvent && oEvent.getSource && oEvent.getSource(), "dialogKind")
            || readSourceFlag(oEvent && oEvent.getSource && oEvent.getSource(), "kind");
        return sKind === "barrier" ? "barrier" : "check";
    }

    function buildHooks(oController) {
        return {
            withViewFlag: function (sPath, fnTask) { return oController._withViewFlag(sPath, fnTask); },
            rowOps: function (mInput) { return DetailCommandPolicy.rowOps(oController, mInput); },
            resolveRowInput: function (oEvent) { return oController._resolveRowInput(oEvent); },
            rememberDialogReturnFocus: function (sDialogKey, oSource) { oController._rememberDialogReturnFocus(sDialogKey, oSource); },
            writeCards: function (aCards, sDraggedKey) { DetailInfoCardLayoutRuntime.writeCards(oController, aCards, sDraggedKey); },
            togglePin: function (sKey) { DetailInfoCardLayoutRuntime.togglePin(oController, sKey); },
            moveCard: function (sKey, iDelta) { DetailInfoCardLayoutRuntime.moveCard(oController, sKey, iDelta); },
            focusCardByKey: function (sKey) { DetailInfoCardLayoutRuntime.focusCardByKey(oController, sKey); },
            readContextValue: function (oContext, sKey, vFallback) { return BindingContextReader.read(oContext, sKey, vFallback); },
            isEditMode: function () { return oController._isEditMode(); }
        };
    }

    function resolveHooks(oController) {
        if (!oController._hooks) {
            oController._hooks = buildHooks(oController);
        }
        return oController._hooks;
    }

    return {
        onAddChecklistSection: function (oEvent) { return DetailRowBehaviorRuntime.onAddRow(this, resolveRowKind(oEvent), resolveHooks(this)); },
        onAddCheckRow: function () { return DetailRowBehaviorRuntime.onAddRow(this, "check", resolveHooks(this)); },
        onAddBarrierRow: function () { return DetailRowBehaviorRuntime.onAddRow(this, "barrier", resolveHooks(this)); },
        onDeleteCheckRow: function (oEvent) { return DetailRowBehaviorRuntime.onDeleteRow(this, "check", oEvent, resolveHooks(this)); },
        onDeleteBarrierRow: function (oEvent) { return DetailRowBehaviorRuntime.onDeleteRow(this, "barrier", oEvent, resolveHooks(this)); },
        onDeleteExpandedRow: function (oEvent) { return DetailRowBehaviorRuntime.onDeleteRow(this, resolveRowKind(oEvent), oEvent, resolveHooks(this)); },
        onExpandChecklistSection: function (oEvent) { return DetailRowBehaviorRuntime.onExpandRows(this, resolveRowKind(oEvent), oEvent, resolveHooks(this)); },
        onExpandChecks: function (oEvent) { return DetailRowBehaviorRuntime.onExpandRows(this, "check", oEvent, resolveHooks(this)); },
        onExpandBarriers: function (oEvent) { return DetailRowBehaviorRuntime.onExpandRows(this, "barrier", oEvent, resolveHooks(this)); },
        onCloseChecksExpanded: function () { return DetailRowBehaviorRuntime.onCloseRowsExpanded(this, "check", resolveHooks(this)); },
        onCloseBarriersExpanded: function () { return DetailRowBehaviorRuntime.onCloseRowsExpanded(this, "barrier", resolveHooks(this)); },
        onCloseExpandedRows: function (oEvent) { return DetailRowBehaviorRuntime.onCloseRowsExpanded(this, resolveRowKind(oEvent), resolveHooks(this)); },
        onInfoCardsDrop: function (oEvent) { DetailRowBehaviorRuntime.onInfoCardsDrop(this, oEvent, resolveHooks(this)); },
        onToggleInfoCardPin: function (oEvent) { DetailRowBehaviorRuntime.onToggleInfoCardPin(this, oEvent, resolveHooks(this)); },
        onInfoCardPress: function (oEvent, sCardKey, oItem) { DetailRowBehaviorRuntime.onInfoCardPress(this, oEvent, sCardKey, oItem, resolveHooks(this)); },
        onInfoCardKeyDown: function (oEvent, sCardKey) { DetailRowBehaviorRuntime.onInfoCardKeyDown(this, oEvent, sCardKey, resolveHooks(this)); },
        onConfirmTestUser: function () { DetailCommandPolicy.resolveConflict(this, { intent: DETAIL_SOURCES.TEST_USER }); },
        onSelectionToggle: function () { DetailCommandPolicy.rowOps(this, { op: "selectionToggle" }); },
        onRowValueChange: function (oEvent) { this._applyDetailFieldChange(oEvent, { property: "value", parameter: "value" }); },
        onRowStateChange: function (oEvent) { this._applyDetailFieldChange(oEvent, { property: "state", parameter: "state" }); },
        onDialogClosed: function () { DetailCommandPolicy.resolveConflict(this, { intent: DETAIL_SOURCES.DIALOG_CLOSED }); }
    };
});
