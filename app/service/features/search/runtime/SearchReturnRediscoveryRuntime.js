sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionStateRuntime"
], function (ControllerViewStateRuntime, ModelStateRuntime, ModelContracts, ModelPathContracts, ChecklistIdentity, SearchSelectionStateRuntime) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var MODES = Object.freeze({
        CREATE: "create",
        DELETE: "delete",
        SAVE: "save"
    });

    function normalizeString(vValue) {
        return String(vValue || "").trim();
    }

    function normalizeBoolean(vValue, bFallback) {
        return typeof vValue === "boolean" ? vValue : !!bFallback;
    }

    function normalizeMode(vValue) {
        var sMode = normalizeString(vValue).toLowerCase();
        if (sMode === MODES.CREATE || sMode === MODES.DELETE || sMode === MODES.SAVE) {
            return sMode;
        }
        return "";
    }

    function buildContext(mInput) {
        var oContext = mInput || {};
        var sRootId = normalizeString(oContext.rootId);
        var sChecklistId = normalizeString(oContext.checklistId);
        var sMode = normalizeMode(oContext.mode);
        if (!sMode && !sRootId && !sChecklistId) {
            return null;
        }
        return {
            rootId: sRootId,
            checklistId: sChecklistId,
            reason: normalizeString(oContext.reason),
            mode: sMode || MODES.SAVE,
            focusRequested: normalizeBoolean(oContext.focusRequested, false),
            selectionRequested: normalizeBoolean(oContext.selectionRequested, true)
        };
    }

    function readContext(oController) {
        return buildContext(ModelStateRuntime.read(
            oController,
            STATE_MODEL,
            ModelPathContracts.SEARCH_RETURN_CONTEXT,
            null
        ));
    }

    function clearContext(oController) {
        ModelStateRuntime.write(oController, STATE_MODEL, ModelPathContracts.SEARCH_RETURN_CONTEXT, null);
    }

    function hasLegacyRefreshFlag(_oController) {
        return false;
    }

    function extractItemObject(oItem) {
        var oCtx = oItem && oItem.getBindingContext && oItem.getBindingContext();
        return oCtx && oCtx.getObject && oCtx.getObject();
    }

    function resolveVisibleItems(oInnerTable) {
        return oInnerTable && oInnerTable.getItems ? (oInnerTable.getItems() || []) : [];
    }

    function findMatchingItem(oInnerTable, oReturnContext) {
        var sRootId = normalizeString(oReturnContext && oReturnContext.rootId);
        var sChecklistId = normalizeString(oReturnContext && oReturnContext.checklistId);
        return resolveVisibleItems(oInnerTable).find(function (oItem) {
            var oObject = extractItemObject(oItem);
            var sRowRootId = ChecklistIdentity.extractChecklistId(oObject);
            var sRowChecklistId = ChecklistIdentity.extractChecklistDisplayId(oObject);
            if (sRootId && sRowRootId === sRootId) {
                return true;
            }
            return !!sChecklistId && sRowChecklistId === sChecklistId;
        }) || null;
    }

    function applySelectionState(oController, oOptions) {
        var mSelectionState = SearchSelectionStateRuntime.buildSelectionStatePayload(
            oOptions && oOptions.selectedRowId ? [oOptions.selectedRowId] : [],
            oOptions && oOptions.selectedRowDisplayId
        );

        ControllerViewStateRuntime.setMany(oController, {
            "/selectedRowId": mSelectionState.selectedRowId,
            "/selectedRowDisplayId": mSelectionState.selectedRowDisplayId,
            "/selectedRowIds": mSelectionState.selectedRowIds,
            "/selectionCount": mSelectionState.selectionCount,
            "/hasSelection": mSelectionState.hasSelection,
            "/canCopy": mSelectionState.canCopy,
            "/canDelete": mSelectionState.canDelete
        });
    }

    function clearTableSelection(oInnerTable) {
        if (oInnerTable && oInnerTable.removeSelections) {
            oInnerTable.removeSelections(true);
        }
    }

    function selectTableItem(oInnerTable, oItem) {
        clearTableSelection(oInnerTable);
        if (!oInnerTable || !oItem) {
            return;
        }
        if (typeof oInnerTable.setSelectedItem === "function") {
            oInnerTable.setSelectedItem(oItem, true);
            return;
        }
        if (typeof oItem.setSelected === "function") {
            oItem.setSelected(true);
        }
    }

    function scrollItemIntoView(oItem) {
        var oDomRef = oItem && oItem.getDomRef && oItem.getDomRef();
        if (oDomRef && typeof oDomRef.scrollIntoView === "function") {
            oDomRef.scrollIntoView({ block: "nearest", inline: "nearest" });
        }
    }

    function handleDeleteReturn(oController, oInnerTable) {
        clearTableSelection(oInnerTable);
        applySelectionState(oController, {});
    }

    function handleFoundReturnItem(oController, oInnerTable, oItem, oReturnContext) {
        var oObject = extractItemObject(oItem);
        var sSelectedRowId = ChecklistIdentity.extractChecklistId(oObject) || normalizeString(oReturnContext.rootId);
        var sSelectedRowDisplayId = ChecklistIdentity.extractChecklistDisplayId(oObject) || normalizeString(oReturnContext.checklistId);

        if (oReturnContext.selectionRequested !== false) {
            selectTableItem(oInnerTable, oItem);
            applySelectionState(oController, {
                selectedRowId: sSelectedRowId,
                selectedRowDisplayId: sSelectedRowDisplayId
            });
        } else {
            clearTableSelection(oInnerTable);
            applySelectionState(oController, {});
        }

        if (oReturnContext.focusRequested) {
            scrollItemIntoView(oItem);
        }
    }

    function handleMissingReturnItem(oController, oInnerTable) {
        clearTableSelection(oInnerTable);
        applySelectionState(oController, {});
    }

    function applyAfterSearchSuccess(oController, oInnerTable) {
        var oReturnContext = readContext(oController);
        var oMatchedItem;

        if (!oReturnContext) {
            return false;
        }

        if (oReturnContext.mode === MODES.DELETE) {
            handleDeleteReturn(oController, oInnerTable);
            clearContext(oController);
            return true;
        }

        oMatchedItem = findMatchingItem(oInnerTable, oReturnContext);
        if (oMatchedItem) {
            handleFoundReturnItem(oController, oInnerTable, oMatchedItem, oReturnContext);
        } else {
            handleMissingReturnItem(oController, oInnerTable);
        }
        clearContext(oController);
        return true;
    }

    return {
        MODES: MODES,
        applyAfterSearchSuccess: applyAfterSearchSuccess,
        buildContext: buildContext,
        clearContext: clearContext,
        hasLegacyRefreshFlag: hasLegacyRefreshFlag,
        readContext: readContext
    };
});
