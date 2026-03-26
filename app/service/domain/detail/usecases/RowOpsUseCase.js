sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ClientKeyGenerator",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowEntityConfig",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBindingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (StatePaths, Result, Effects, ClientKeyGenerator, DetailRowEntityConfig, DetailRowBindingRuntime, ModelContracts, DetailContracts, MessageKeyConstants) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var DETAIL_MODEL = ModelContracts.MODELS.DETAIL;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.VIEW;

    function RowOpsUseCase() {
        return {
            execute: execute
        };
    }

function nextNum(aRows, sField) {
        return (aRows || []).reduce(function (iMax, oRow) {
            var iVal = Number(oRow && oRow[sField]) || 0;
            return iVal > iMax ? iVal : iMax;
        }, 0) + 1;
    }

    function createClientRowKey() {
        return ClientKeyGenerator.createHex32();
    }

    function resolveRowIndexFromEvent(oEvent, sModelName, sCollectionPath) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        var oContext = DetailRowBindingRuntime.resolveDetailBindingContext(oSource, sModelName);
        var sPath = String((oContext && oContext.getPath && oContext.getPath()) || "");
        var sPrefix = String(sCollectionPath || "");
        var oMatch;
        if (!sPath || !sPrefix || sPath.indexOf(sPrefix + "/") !== 0) {
            return -1;
        }
        oMatch = sPath.match(/\/(\d+)$/);
        return oMatch ? Number(oMatch[1]) : -1;
    }

    function resolveRowIndexFromInput(mInput, aRows, sCollectionPath) {
        var sExplicitPath = String((mInput && mInput.rowPath) || "");
        var sPrefix = String(sCollectionPath || "");
        var oMatch;
        var iIndex = -1;
        var sRowId;
        if (sExplicitPath && sPrefix && sExplicitPath.indexOf(sPrefix + "/") === 0) {
            oMatch = sExplicitPath.match(/\/(\d+)$/);
            if (oMatch) {
                iIndex = Number(oMatch[1]);
                if (iIndex >= 0) {
                    return iIndex;
                }
            }
        }
        sRowId = String((mInput && mInput.rowId) || "").trim();
        if (!sRowId) {
            return -1;
        }
        return (Array.isArray(aRows) ? aRows : []).findIndex(function (oRow) {
            return String((oRow && (oRow.client_row_id || oRow.Key || oRow.id)) || "").trim() === sRowId;
        });
    }

    function removeRows(aRows, iIndex) {
        var aSafe = Array.isArray(aRows) ? aRows : [];
        if (iIndex >= 0 && iIndex < aSafe.length) {
            return aSafe.filter(function (_, iRowIndex) { return iRowIndex !== iIndex; });
        }
        return aSafe.filter(function (oRow) { return !(oRow && oRow.selected); });
    }

    function markDirtyResult(sEntity, sOp) {
        return Result.ok({ entity: sEntity, op: sOp }, [
            Effects.modelPatch(STATE_MODEL, StatePaths.WORKFLOW_DIRTY, true),
            Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_STATE, DetailContracts.STATES.DIRTY),
            Effects.modelPatch(STATE_MODEL, StatePaths.PERSISTENCE_MESSAGE_KEY, DETAIL_MESSAGE_KEYS.PERSISTENCE_DIRTY)
        ]);
    }

    function createRow(aRows, oConfig) {
        var sRowKey = createClientRowKey();
        var oRow = { id: sRowKey, Key: sRowKey, client_row_id: sRowKey, text: "", comment: "", result: false, selected: false };
        oRow[oConfig.numberField] = nextNum(aRows, oConfig.numberField);
        return oRow;
    }

    function handleEntityRowOp(oUiState, sEntity, sOp, mInput) {
        var oConfig = DetailRowEntityConfig.get(sEntity);
        var sRowsPath = DetailRowBindingRuntime.resolveRowsPath(oConfig);
        var aRows = oUiState.get(DETAIL_MODEL, sRowsPath) || [];
        var sExpandedDialogPath;
        var oExpandedDialogState;
        var iRowIndex;
        if (sOp === "add") {
            oUiState.set(DETAIL_MODEL, sRowsPath, aRows.concat([createRow(aRows, oConfig)]));
            return markDirtyResult(sEntity, sOp);
        }
        if (sOp === "delete") {
            iRowIndex = resolveRowIndexFromInput(mInput, aRows, sRowsPath);
            if (iRowIndex < 0) {
                iRowIndex = resolveRowIndexFromEvent(mInput && mInput.event, DETAIL_MODEL, sRowsPath);
            }
            oUiState.set(DETAIL_MODEL, sRowsPath, removeRows(aRows, iRowIndex));
            return markDirtyResult(sEntity, sOp);
        }
        if (sOp === "expand" || sOp === "collapse") {
            sExpandedDialogPath = sEntity === "barrier"
                ? "/detailExpandedDialogs/barriers"
                : "/detailExpandedDialogs/checks";
            oExpandedDialogState = oUiState.get(VIEW_MODEL, sExpandedDialogPath) || null;
            return Result.ok({ entity: sEntity, op: sOp }, [
                Effects.modelPatch(VIEW_MODEL, "/activeExpandedDialog", oExpandedDialogState),
                Effects.dialog(oConfig.dialogId, sOp === "expand" ? "open" : "close", {})
            ]);
        }
        return null;
    }

    function resolveCardKeyFromControl(oControl) {
        var oCursor = oControl;
        while (oCursor) {
            var oCtx = oCursor.getBindingContext && oCursor.getBindingContext(VIEW_MODEL);
            var oData = oCtx && oCtx.getObject && oCtx.getObject();
            if (oData && oData.key) {
                return oData.key;
            }
            oCursor = oCursor.getParent && oCursor.getParent();
        }
        return "";
    }

    function moveCard(aCards, sDraggedKey, sDroppedKey, sDropPosition) {
        var aSafe = Array.isArray(aCards) ? aCards.slice() : [];
        var iFrom = aSafe.findIndex(function (oItem) { return oItem && oItem.key === sDraggedKey; });
        var iTo = aSafe.findIndex(function (oItem) { return oItem && oItem.key === sDroppedKey; });
        var sPosition = String(sDropPosition || "").toLowerCase();
        var oDragged;
        var iInsert;
        if (iFrom < 0 || iTo < 0 || iFrom === iTo) {
            return aSafe;
        }
        if (sPosition === "on") {
            oDragged = aSafe[iFrom];
            aSafe[iFrom] = aSafe[iTo];
            aSafe[iTo] = oDragged;
            return aSafe;
        }
        oDragged = aSafe.splice(iFrom, 1)[0];
        if (iFrom < iTo) {
            iTo -= 1;
        }
        iInsert = sPosition === "after" ? iTo + 1 : iTo;
        if (iInsert < 0) { iInsert = 0; }
        if (iInsert > aSafe.length) { iInsert = aSafe.length; }
        aSafe.splice(iInsert, 0, oDragged);
        return aSafe;
    }

    function execute(mInput, mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var sEntity = (mInput && mInput.entity) || "";
        var sOp = (mInput && mInput.op) || "";
        var oResult;
        if (!oUiState) {
            return Promise.resolve(Result.fail({ message: "UiState unavailable", code: "UISTATE_UNAVAILABLE" }));
        }
        if (DetailRowEntityConfig.all[sEntity]) {
            oResult = handleEntityRowOp(oUiState, sEntity, sOp, mInput);
            if (oResult) {
                return Promise.resolve(oResult);
            }
        }
        if (sEntity === "card" && sOp === "drop") {
            var oEvent = mInput && mInput.event;
            var sDropPosition = oEvent && oEvent.getParameter && oEvent.getParameter("dropPosition");
            var sDraggedKey = resolveCardKeyFromControl(oEvent && oEvent.getParameter && oEvent.getParameter("draggedControl"));
            var sDroppedKey = resolveCardKeyFromControl(oEvent && oEvent.getParameter && oEvent.getParameter("droppedControl"));
            var aCards = oUiState.get(VIEW_MODEL, "/infoCards") || [];
            oUiState.set(VIEW_MODEL, "/infoCards", moveCard(aCards, sDraggedKey, sDroppedKey, sDropPosition));
            return Promise.resolve(Result.ok({ entity: sEntity, op: sOp }, []));
        }
        return Promise.resolve(Result.ok({ entity: sEntity, op: sOp }, [
            Effects.log("info", "Row operation", { entity: sEntity, op: sOp })
        ]));
    }

    return RowOpsUseCase;
});
