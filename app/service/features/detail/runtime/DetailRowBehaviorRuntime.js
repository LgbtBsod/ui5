sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowEntityConfig"
], function (ControllerViewStateRuntime, ModelStateRuntime, StatePaths, WorkflowContracts, DetailRowEntityConfig) {
    "use strict";

    function runRowOperation(oController, sEntity, sOp, mInput, mHooks) {
        var oConfig = DetailRowEntityConfig.get(sEntity);
        var sBusyPath = (sOp === "expand" || sOp === "collapse") ? oConfig.dialogBusyPath : oConfig.rowBusyPath;
        var sBeforeMode = WorkflowContracts.normalizeEditMode(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ));
        var sBeforeLockState = WorkflowContracts.normalizeLockState(ModelStateRuntime.read(oController, "state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY));

        if (!sBusyPath || !mHooks || typeof mHooks.withViewFlag !== "function" || typeof mHooks.rowOps !== "function") {
            return Promise.resolve();
        }
        return mHooks.withViewFlag(sBusyPath, function () {
            return mHooks.rowOps(Object.assign({ entity: sEntity, op: sOp }, mInput || {}));
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

    function reorderInfoCards(oController, oEvent, mHooks) {
        var oDragged = oEvent && oEvent.getParameter && oEvent.getParameter("draggedControl");
        var oDropped = oEvent && oEvent.getParameter && oEvent.getParameter("droppedControl");
        var oDraggedContext = oDragged && oDragged.getBindingContext && oDragged.getBindingContext("view");
        var oDroppedContext = oDropped && oDropped.getBindingContext && oDropped.getBindingContext("view");
        var aCards = (ControllerViewStateRuntime.get(oController, "/infoCards") || []).map(function (oCard) {
            return Object.assign({}, oCard);
        });
        var iDraggedIndex;
        var iDroppedIndex;
        var oDraggedCard;
        var sDraggedKey;

        if (!oDraggedContext || !oDroppedContext || !aCards.length || !mHooks || typeof mHooks.writeCards !== "function") {
            return;
        }
        sDraggedKey = mHooks.readContextValue(oDraggedContext, "key", "");
        iDraggedIndex = aCards.findIndex(function (oCard) {
            return oCard.key === sDraggedKey;
        });
        iDroppedIndex = aCards.findIndex(function (oCard) {
            return oCard.key === mHooks.readContextValue(oDroppedContext, "key", "");
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
        mHooks.writeCards(aCards, sDraggedKey);
    }

    return {
        entityConfig: DetailRowEntityConfig.all,
        onAddRow: function (oController, sEntity, mHooks) {
            return runRowOperation(oController, sEntity, "add", null, mHooks);
        },
        onDeleteRow: function (oController, sEntity, oEvent, mHooks) {
            return runRowOperation(oController, sEntity, "delete", Object.assign({ event: oEvent }, mHooks.resolveRowInput(oEvent)), mHooks);
        },
        onExpandRows: function (oController, sEntity, oEvent, mHooks) {
            mHooks.rememberDialogReturnFocus(DetailRowEntityConfig.get(sEntity).dialogId, oEvent && oEvent.getSource && oEvent.getSource());
            return runRowOperation(oController, sEntity, "expand", null, mHooks);
        },
        onCloseRowsExpanded: function (oController, sEntity, mHooks) {
            return runRowOperation(oController, sEntity, "collapse", null, mHooks);
        },
        onInfoCardsDrop: function (oController, oEvent, mHooks) {
            reorderInfoCards(oController, oEvent, mHooks);
        },
        onToggleInfoCardPin: function (oController, oEvent, mHooks) {
            var oSource = oEvent && oEvent.getSource && oEvent.getSource();
            var oContext = oSource && oSource.getBindingContext && oSource.getBindingContext("view");
            if (!oContext || !mHooks || typeof mHooks.togglePin !== "function") {
                return;
            }
            mHooks.togglePin(mHooks.readContextValue(oContext, "key", ""));
        },
        onInfoCardPress: function (oController, oEvent, sCardKey, oItem, mHooks) {
            var oTarget = oEvent && oEvent.target;
            var oDomRef;
            var oFocusable;
            var sNormalized = String(sCardKey || "").trim();
            if (!sNormalized || !mHooks.isEditMode() || isInteractiveTarget(oTarget)) {
                return;
            }
            oDomRef = oItem && oItem.getDomRef ? oItem.getDomRef() : null;
            oFocusable = findFocusableField(oDomRef);
            if (oFocusable && typeof oFocusable.focus === "function") {
                oFocusable.focus();
                return;
            }
            mHooks.focusCardByKey(sNormalized);
        },
        onInfoCardKeyDown: function (oController, oEvent, sCardKey, mHooks) {
            var bModifier = !!(oEvent && oEvent.ctrlKey && oEvent.shiftKey);
            var sKey = String(oEvent && oEvent.key || "");
            var sNormalized = String(sCardKey || "").trim();
            if (!sNormalized || !mHooks.isEditMode()) {
                return;
            }
            if (bModifier && (sKey === "ArrowUp" || sKey === "ArrowLeft")) {
                oEvent.preventDefault();
                mHooks.moveCard(sNormalized, -1);
                return;
            }
            if (bModifier && (sKey === "ArrowDown" || sKey === "ArrowRight")) {
                oEvent.preventDefault();
                mHooks.moveCard(sNormalized, 1);
                return;
            }
            if (bModifier && (sKey === "p" || sKey === "P")) {
                oEvent.preventDefault();
                mHooks.togglePin(sNormalized);
            }
        }
    };
});
