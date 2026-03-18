sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/BindingContextReader",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBehaviorRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/OperationSourceContracts"
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
            || readSourceFlag(oEvent && oEvent.getSource && oEvent.getSource(), "dialogKind");
        return sKind === "barrier" ? "barrier" : "check";
    }

    function createHooks(oController) {
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

    return {
        onAddCheckRow: function () { return DetailRowBehaviorRuntime.onAddRow(this, "check", createHooks(this)); },
        onAddBarrierRow: function () { return DetailRowBehaviorRuntime.onAddRow(this, "barrier", createHooks(this)); },
        onDeleteCheckRow: function (oEvent) { return DetailRowBehaviorRuntime.onDeleteRow(this, "check", oEvent, createHooks(this)); },
        onDeleteBarrierRow: function (oEvent) { return DetailRowBehaviorRuntime.onDeleteRow(this, "barrier", oEvent, createHooks(this)); },
        onDeleteExpandedRow: function (oEvent) { return DetailRowBehaviorRuntime.onDeleteRow(this, resolveRowKind(oEvent), oEvent, createHooks(this)); },
        onExpandChecks: function (oEvent) { return DetailRowBehaviorRuntime.onExpandRows(this, "check", oEvent, createHooks(this)); },
        onExpandBarriers: function (oEvent) { return DetailRowBehaviorRuntime.onExpandRows(this, "barrier", oEvent, createHooks(this)); },
        onCloseChecksExpanded: function () { return DetailRowBehaviorRuntime.onCloseRowsExpanded(this, "check", createHooks(this)); },
        onCloseBarriersExpanded: function () { return DetailRowBehaviorRuntime.onCloseRowsExpanded(this, "barrier", createHooks(this)); },
        onCloseExpandedRows: function (oEvent) { return DetailRowBehaviorRuntime.onCloseRowsExpanded(this, resolveRowKind(oEvent), createHooks(this)); },
        onInfoCardsDrop: function (oEvent) { DetailRowBehaviorRuntime.onInfoCardsDrop(this, oEvent, createHooks(this)); },
        onToggleInfoCardPin: function (oEvent) { DetailRowBehaviorRuntime.onToggleInfoCardPin(this, oEvent, createHooks(this)); },
        onInfoCardPress: function (oEvent, sCardKey, oItem) { DetailRowBehaviorRuntime.onInfoCardPress(this, oEvent, sCardKey, oItem, createHooks(this)); },
        onInfoCardKeyDown: function (oEvent, sCardKey) { DetailRowBehaviorRuntime.onInfoCardKeyDown(this, oEvent, sCardKey, createHooks(this)); },
        onConfirmTestUser: function () { DetailCommandPolicy.resolveConflict(this, { intent: DETAIL_SOURCES.TEST_USER }); },
        onSelectionToggle: function () { DetailCommandPolicy.rowOps(this, { op: "selectionToggle" }); },
        onRowValueChange: function (oEvent) { this._applySelectedFieldChange(oEvent, { property: "value", parameter: "value" }); },
        onRowStateChange: function (oEvent) { this._applySelectedFieldChange(oEvent, { property: "state", parameter: "state" }); },
        onDialogClosed: function () { DetailCommandPolicy.resolveConflict(this, { intent: DETAIL_SOURCES.DIALOG_CLOSED }); }
    };
});
