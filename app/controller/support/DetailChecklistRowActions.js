sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/BindingContextReadSupport",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/WorkflowContracts"
], function (BindingContextReadSupport, DetailCommandPolicy, ControllerViewStateRuntime, DetailInfoCardLayoutRuntime, ModelStateRuntime, StatePaths, WorkflowContracts) {
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
        var sBeforeMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sBeforeLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
        if (!sBusyPath) {
            return Promise.resolve();
        }
        return oController._withViewFlag(sBusyPath, function () {
            return DetailCommandPolicy.rowOps(oController, Object.assign({ entity: sEntity, op: sOp }, mInput || {}));
        }).then(function (vResult) {
            var sCurrentMode;
            var sCurrentLockState;
            if (sOp !== "expand" && sOp !== "collapse") {
                return vResult;
            }
            sCurrentMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
            sCurrentLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));
            if (WorkflowContracts.isEditableMode(sBeforeMode) &&
                sCurrentMode === WorkflowContracts.EDIT_MODES.READ &&
                ((sBeforeMode === WorkflowContracts.EDIT_MODES.EDIT && sCurrentLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED && sBeforeLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED) ||
                    sBeforeMode === WorkflowContracts.EDIT_MODES.CREATE)) {
                ModelStateRuntime.write(oController, "state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, sBeforeMode);
            }
            return vResult;
        });
    }

    function findFocusableField(oRoot) {
        if (!oRoot || !oRoot.querySelector) {
            return null;
        }
        return oRoot.querySelector([
            "input:not([disabled]):not([readonly])",
            "textarea:not([disabled]):not([readonly])",
            "[role='combobox']:not([aria-disabled='true'])",
            ".sapMSlt:not(.sapMSltDisabled)",
            ".sapMSwt:not(.sapMSwtDisabled)"
        ].join(","));
    }

    function isInteractiveTarget(oTarget) {
        if (!oTarget || !oTarget.closest) {
            return false;
        }
        return !!oTarget.closest([
            ".sapMInputBase",
            ".sapMSlt",
            ".sapMBtn",
            ".sapMSwt",
            "input",
            "textarea",
            "select",
            "button",
            "a",
            "[role='button']",
            "[role='switch']",
            "[role='combobox']"
        ].join(","));
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
            var aCards = (ControllerViewStateRuntime.get(this, "/infoCards") || []).map(function (oCard) {
                return Object.assign({}, oCard);
            });
            var iDraggedIndex;
            var iDroppedIndex;
            var oDraggedCard;
            var sDraggedKey;
            if (!oDraggedContext || !oDroppedContext || !aCards.length) {
                return;
            }
            sDraggedKey = BindingContextReadSupport.read(oDraggedContext, "key", "");
            iDraggedIndex = aCards.findIndex(function (oCard) {
                return oCard.key === sDraggedKey;
            });
            iDroppedIndex = aCards.findIndex(function (oCard) {
                return oCard.key === BindingContextReadSupport.read(oDroppedContext, "key", "");
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
            DetailInfoCardLayoutRuntime.writeCards(this, aCards, sDraggedKey);
        },

        onToggleInfoCardPin: function (oEvent) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var oContext = oSource && oSource.getBindingContext && oSource.getBindingContext("view");
            if (!oContext) {
                return;
            }
            DetailInfoCardLayoutRuntime.togglePin(this, BindingContextReadSupport.read(oContext, "key", ""));
        },

        onInfoCardPress: function (oEvent, sCardKey, oItem) {
            var oTarget = oEvent && oEvent.target;
            var oDomRef;
            var oFocusable;
            var sNormalized = String(sCardKey || "").trim();
            if (!sNormalized || !this._isEditMode() || isInteractiveTarget(oTarget)) {
                return;
            }
            oDomRef = oItem && oItem.getDomRef ? oItem.getDomRef() : null;
            oFocusable = findFocusableField(oDomRef);
            if (oFocusable && typeof oFocusable.focus === "function") {
                oFocusable.focus();
                return;
            }
            DetailInfoCardLayoutRuntime.focusCardByKey(this, sNormalized);
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
                DetailInfoCardLayoutRuntime.moveCard(this, sNormalized, -1);
                return;
            }
            if (bModifier && (sKey === "ArrowDown" || sKey === "ArrowRight")) {
                oEvent.preventDefault();
                DetailInfoCardLayoutRuntime.moveCard(this, sNormalized, 1);
                return;
            }
            if (bModifier && (sKey === "p" || sKey === "P")) {
                oEvent.preventDefault();
                DetailInfoCardLayoutRuntime.togglePin(this, sNormalized);
            }
        },

        onConfirmTestUser: function () {
            DetailCommandPolicy.resolveConflict(this, { intent: "testUser" });
        },

        onSelectionToggle: function () {
            DetailCommandPolicy.rowOps(this, { op: "selectionToggle" });
        },

        onRowValueChange: function (oEvent) {
            this._applySelectedFieldChange(oEvent, {
                property: "value",
                parameter: "value"
            });
        },

        onRowStateChange: function (oEvent) {
            this._applySelectedFieldChange(oEvent, {
                property: "state",
                parameter: "state"
            });
        },

        onDialogClosed: function () {
            DetailCommandPolicy.resolveConflict(this, { intent: "dialogClosed" });
        }
    };
});
