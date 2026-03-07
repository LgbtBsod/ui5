sap.ui.define([
    "sap_ui5/controller/support/DetailCommandPolicy",
    "sap_ui5/controller/support/ControllerModelWriteSupport",
    "sap_ui5/controller/support/DetailInfoCardLayoutSupport"
], function (DetailCommandPolicy, ControllerModelWriteSupport, DetailInfoCardLayoutSupport) {
    "use strict";

    var ROW_ENTITY_CONFIG = {
        check: {
            rowBusyPath: "/checksBusy",
            dialogBusyPath: "/checksExpandedBusy",
            dialogKey: "checksExpanded"
        },
        barrier: {
            rowBusyPath: "/barriersBusy",
            dialogBusyPath: "/barriersExpandedBusy",
            dialogKey: "barriersExpanded"
        }
    };

    function runRowOperation(oController, sEntity, sOp, mInput) {
        var oConfig = ROW_ENTITY_CONFIG[sEntity] || {};
        var sBusyPath = (sOp === "expand" || sOp === "collapse") ? oConfig.dialogBusyPath : oConfig.rowBusyPath;
        if (!sBusyPath) {
            return Promise.resolve();
        }
        return oController._withViewFlag(sBusyPath, function () {
            return DetailCommandPolicy.rowOps(oController, Object.assign({ entity: sEntity, op: sOp }, mInput || {}));
        });
    }

    return {
        onAddCheckRow: function () {
            return runRowOperation(this, "check", "add");
        },

        onAddBarrierRow: function () {
            return runRowOperation(this, "barrier", "add");
        },

        onDeleteCheckRow: function (oEvent) {
            var mRow = this._resolveRowInput(oEvent);
            return runRowOperation(this, "check", "delete", Object.assign({ event: oEvent }, mRow));
        },

        onDeleteBarrierRow: function (oEvent) {
            var mRow = this._resolveRowInput(oEvent);
            return runRowOperation(this, "barrier", "delete", Object.assign({ event: oEvent }, mRow));
        },

        onExpandChecks: function (oEvent) {
            this._rememberDialogReturnFocus(ROW_ENTITY_CONFIG.check.dialogKey, oEvent && oEvent.getSource && oEvent.getSource());
            return runRowOperation(this, "check", "expand");
        },

        onExpandBarriers: function (oEvent) {
            this._rememberDialogReturnFocus(ROW_ENTITY_CONFIG.barrier.dialogKey, oEvent && oEvent.getSource && oEvent.getSource());
            return runRowOperation(this, "barrier", "expand");
        },

        onCloseChecksExpanded: function () {
            return runRowOperation(this, "check", "collapse");
        },

        onCloseBarriersExpanded: function () {
            return runRowOperation(this, "barrier", "collapse");
        },

        onInfoCardsDrop: function (oEvent) {
            var oDragged = oEvent && oEvent.getParameter && oEvent.getParameter("draggedControl");
            var oDropped = oEvent && oEvent.getParameter && oEvent.getParameter("droppedControl");
            var oDraggedContext = oDragged && oDragged.getBindingContext && oDragged.getBindingContext("view");
            var oDroppedContext = oDropped && oDropped.getBindingContext && oDropped.getBindingContext("view");
            var aCards = (ControllerModelWriteSupport.get(this, "view", "/infoCards") || []).map(function (oCard) {
                return Object.assign({}, oCard);
            });
            var iDraggedIndex;
            var iDroppedIndex;
            var oDraggedCard;
            var sDraggedKey;
            if (!oDraggedContext || !oDroppedContext || !aCards.length) {
                return;
            }
            sDraggedKey = oDraggedContext.getProperty("key");
            iDraggedIndex = aCards.findIndex(function (oCard) {
                return oCard.key === sDraggedKey;
            });
            iDroppedIndex = aCards.findIndex(function (oCard) {
                return oCard.key === oDroppedContext.getProperty("key");
            });
            if (iDraggedIndex < 0 || iDroppedIndex < 0 || iDraggedIndex === iDroppedIndex) {
                return;
            }
            oDraggedCard = aCards[iDraggedIndex];
            aCards.splice(iDraggedIndex, 1);
            if (iDraggedIndex < iDroppedIndex) {
                iDroppedIndex -= 1;
            }
            aCards.splice(iDroppedIndex, 0, oDraggedCard);
            DetailInfoCardLayoutSupport.writeCards(this, aCards, sDraggedKey);
        },

        onToggleInfoCardPin: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var oContext = oSource && oSource.getBindingContext && oSource.getBindingContext("view");
            if (!oContext) {
                return;
            }
            DetailInfoCardLayoutSupport.togglePin(this, oContext.getProperty("key"));
        },

        onInfoCardKeyDown: function (oEvent, sCardKey) {
            var bModifier = !!(oEvent && oEvent.ctrlKey && oEvent.shiftKey);
            var sKey = String(oEvent && oEvent.key || "");
            var sNormalized = String(sCardKey || "").trim();
            if (!sNormalized || !this._isEditMode()) {
                return;
            }
            if (bModifier && (sKey === "ArrowUp" || sKey === "ArrowLeft")) {
                oEvent.preventDefault();
                DetailInfoCardLayoutSupport.moveCard(this, sNormalized, -1);
                return;
            }
            if (bModifier && (sKey === "ArrowDown" || sKey === "ArrowRight")) {
                oEvent.preventDefault();
                DetailInfoCardLayoutSupport.moveCard(this, sNormalized, 1);
                return;
            }
            if (bModifier && (sKey === "p" || sKey === "P")) {
                oEvent.preventDefault();
                DetailInfoCardLayoutSupport.togglePin(this, sNormalized);
            }
        },

        onConfirmTestUser: function () {
            DetailCommandPolicy.resolveConflict(this, { intent: "testUser" });
        },

        onSelectionToggle: function () {
            DetailCommandPolicy.rowOps(this, { op: "selectionToggle" });
        },

        onDialogClosed: function () {
            DetailCommandPolicy.resolveConflict(this, { intent: "dialogClosed" });
        },

        onCloseWorkflowAnalytics: function () {
            return null;
        }
    };
});
