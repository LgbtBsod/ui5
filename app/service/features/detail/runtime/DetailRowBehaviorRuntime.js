sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowEntityConfig",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (ControllerViewStateRuntime, DetailRowEntityConfig, FocusRuntime, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function runRowOperation(oController, sEntity, sOp, mInput, mHooks) {
        var oConfig = DetailRowEntityConfig.get(sEntity);
        var sBusyPath = (sOp === "expand" || sOp === "collapse") ? oConfig.dialogBusyPath : oConfig.rowBusyPath;

        if (!sBusyPath || !mHooks || typeof mHooks.withViewFlag !== TYPE_FUNCTION || typeof mHooks.rowOps !== TYPE_FUNCTION) {
            return Promise.resolve();
        }
        return mHooks.withViewFlag(sBusyPath, function () {
            return mHooks.rowOps(Object.assign({ entity: sEntity, op: sOp }, mInput || {}));
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

    function focusCardTarget(oItem, sCardKey, mHooks) {
        var oDomRef;
        var oFocusable;
        if (mHooks && typeof mHooks.focusCardFieldByKey === TYPE_FUNCTION && mHooks.focusCardFieldByKey(sCardKey)) {
            return true;
        }
        if (oItem && FocusRuntime.focusSoon(oItem)) {
            return true;
        }
        oDomRef = oItem && typeof oItem[METHODS.GET_DOM_REF] === TYPE_FUNCTION ? oItem[METHODS.GET_DOM_REF]() : null;
        oFocusable = findFocusableField(oDomRef);
        if (oFocusable && typeof oFocusable[METHODS.FOCUS] === TYPE_FUNCTION) {
            oFocusable[METHODS.FOCUS]();
            return true;
        }
        if (mHooks && typeof mHooks.focusCardByKey === TYPE_FUNCTION) {
            return !!mHooks.focusCardByKey(sCardKey);
        }
        return false;
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
            var sNormalized = String(sCardKey || "").trim();
            if (!sNormalized || !mHooks.isEditMode() || isInteractiveTarget(oTarget)) {
                return;
            }
            focusCardTarget(oItem, sNormalized, mHooks);
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
