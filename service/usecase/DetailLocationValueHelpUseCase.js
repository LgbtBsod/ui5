sap.ui.define([], function () {
    "use strict";

    function openValueHelp(mArgs) {
        var oDialog = mArgs && mArgs.dialog;
        if (!oDialog || typeof oDialog.open !== "function") {
            return false;
        }

        var aRows = (mArgs && mArgs.locations) || [];
        var fnBuildTree = mArgs && mArgs.buildLocationTree;
        var oViewModel = mArgs && mArgs.viewModel;
        if (oViewModel && typeof oViewModel.setProperty === "function" && typeof fnBuildTree === "function") {
            oViewModel.setProperty("/locationVhTree", fnBuildTree(aRows));
        }

        oDialog.open();
        return true;
    }

    function closeValueHelp(mArgs) {
        var oDialog = mArgs && mArgs.dialog;
        if (!oDialog || typeof oDialog.close !== "function") {
            return false;
        }
        oDialog.close();
        return true;
    }

    function buildFilteredLocationTree(mArgs) {
        var sQuery = String((mArgs && mArgs.query) || "");
        var aRows = (mArgs && mArgs.locations) || [];
        var fnNormalize = mArgs && mArgs.normalizeText;
        var fnBuildTree = mArgs && mArgs.buildLocationTree;

        if (typeof fnBuildTree !== "function") {
            return [];
        }

        var sValue = typeof fnNormalize === "function" ? fnNormalize(sQuery) : sQuery.trim().toLowerCase();
        if (!sValue) {
            return fnBuildTree(aRows);
        }

        var mById = {};
        aRows.forEach(function (oRow) {
            if (oRow && oRow.node_id) {
                mById[oRow.node_id] = oRow;
            }
        });

        var mKeep = {};
        aRows.forEach(function (oRow) {
            var sNodeId = typeof fnNormalize === "function" ? fnNormalize(oRow && oRow.node_id) : String((oRow && oRow.node_id) || "").trim().toLowerCase();
            var sParentId = typeof fnNormalize === "function" ? fnNormalize(oRow && oRow.parent_id) : String((oRow && oRow.parent_id) || "").trim().toLowerCase();
            var sName = typeof fnNormalize === "function" ? fnNormalize(oRow && oRow.location_name) : String((oRow && oRow.location_name) || "").trim().toLowerCase();
            var bMatch = sNodeId.indexOf(sValue) >= 0 || sParentId.indexOf(sValue) >= 0 || sName.indexOf(sValue) >= 0;
            if (!bMatch || !oRow || !oRow.node_id) {
                return;
            }
            var sCursor = oRow.node_id;
            while (sCursor && mById[sCursor]) {
                mKeep[sCursor] = true;
                sCursor = mById[sCursor].parent_id;
            }
        });

        var aFiltered = aRows.filter(function (oRow) {
            return !!(oRow && oRow.node_id && mKeep[oRow.node_id]);
        });

        return fnBuildTree(aFiltered);
    }

    function applyLocationSelection(mArgs) {
        var oNode = mArgs && mArgs.node;
        var oSelectedModel = mArgs && mArgs.selectedModel;
        if (!oNode || !oSelectedModel || typeof oSelectedModel.setProperty !== "function") {
            return false;
        }

        oSelectedModel.setProperty("/basic/LOCATION_KEY", oNode.node_id || "");
        oSelectedModel.setProperty("/basic/LOCATION_NAME", oNode.location_name || "");
        oSelectedModel.setProperty("/basic/LOCATION_TEXT", oNode.location_name || "");

        if (typeof mArgs.onAfterApply === "function") {
            mArgs.onAfterApply();
        }
        return true;
    }

    function resolveNodeFromTreeSelectionEvent(oEvent) {
        var oContext = oEvent && oEvent.getParameter ? oEvent.getParameter("rowContext") : null;
        if (oContext && oContext.getObject) {
            return oContext.getObject();
        }

        var oTable = oEvent && oEvent.getSource ? oEvent.getSource() : null;
        if (!oTable || typeof oTable.getSelectedIndex !== "function" || typeof oTable.getContextByIndex !== "function") {
            return null;
        }
        var iIndex = oTable.getSelectedIndex();
        if (iIndex < 0) {
            return null;
        }
        var oSelectedContext = oTable.getContextByIndex(iIndex);
        return oSelectedContext && oSelectedContext.getObject ? oSelectedContext.getObject() : null;
    }

    function resolveNodeFromComboChangeEvent(oEvent, sModelName) {
        var oItem = oEvent && oEvent.getParameter ? oEvent.getParameter("selectedItem") : null;
        if (!oItem || typeof oItem.getBindingContext !== "function") {
            return null;
        }
        var oCtx = oItem.getBindingContext(sModelName || "mpl");
        return oCtx && oCtx.getObject ? oCtx.getObject() : null;
    }


    function runOpenValueHelpLifecycle(mArgs) {
        openValueHelp({
            dialog: mArgs.dialog,
            locations: mArgs.locations,
            viewModel: mArgs.viewModel,
            buildLocationTree: mArgs.buildLocationTree
        });

        if (mArgs.table && typeof mArgs.table.clearSelection === "function") {
            mArgs.table.clearSelection();
        }

        return Promise.resolve(mArgs.ensureLocationsLoaded()).then(function (aLocations) {
            applyFilteredTreeToViewModel({
                query: "",
                locations: aLocations || [],
                viewModel: mArgs.viewModel,
                buildLocationTree: mArgs.buildLocationTree,
                normalizeText: mArgs.normalizeText
            });
            return { ok: true, reason: "value_help_opened" };
        });
    }

    function runListSelectionLifecycle(mArgs) {
        var oItem = mArgs.event && mArgs.event.getParameter ? mArgs.event.getParameter("listItem") : null;
        if (!oItem || typeof oItem.getBindingContext !== "function") {
            return { ok: false, reason: "missing_list_item" };
        }
        var oCtx = oItem.getBindingContext("view");
        var oNode = oCtx && oCtx.getObject ? oCtx.getObject() : null;
        if (!oNode) {
            return { ok: false, reason: "missing_node" };
        }
        applyLocationSelection({ node: oNode, selectedModel: mArgs.selectedModel, onAfterApply: mArgs.onAfterApply });
        return { ok: true, reason: "selection_applied" };
    }

    function runTreeSelectionLifecycle(mArgs) {
        var oNode = resolveNodeFromTreeSelectionEvent(mArgs.event);
        if (!oNode) {
            return { ok: false, reason: "missing_node" };
        }
        applyLocationSelection({ node: oNode, selectedModel: mArgs.selectedModel, onAfterApply: mArgs.onAfterApply });
        if (typeof mArgs.onClose === "function") {
            mArgs.onClose();
        }
        return { ok: true, reason: "selection_applied" };
    }

    function runComboSelectionLifecycle(mArgs) {
        var oNode = resolveNodeFromComboChangeEvent(mArgs.event, mArgs.modelName || "mpl");
        if (!oNode) {
            return { ok: false, reason: "missing_node" };
        }
        applyLocationSelection({ node: oNode, selectedModel: mArgs.selectedModel, onAfterApply: mArgs.onAfterApply });
        return { ok: true, reason: "selection_applied" };
    }

    function applyFilteredTreeToViewModel(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        if (!oViewModel || typeof oViewModel.setProperty !== "function") {
            return null;
        }
        var aTree = buildFilteredLocationTree(mArgs);
        oViewModel.setProperty("/locationVhTree", aTree);
        return aTree;
    }

    return {
        openValueHelp: openValueHelp,
        closeValueHelp: closeValueHelp,
        buildFilteredLocationTree: buildFilteredLocationTree,
        applyLocationSelection: applyLocationSelection,
        resolveNodeFromTreeSelectionEvent: resolveNodeFromTreeSelectionEvent,
        resolveNodeFromComboChangeEvent: resolveNodeFromComboChangeEvent,
        runOpenValueHelpLifecycle: runOpenValueHelpLifecycle,
        runListSelectionLifecycle: runListSelectionLifecycle,
        runTreeSelectionLifecycle: runTreeSelectionLifecycle,
        runComboSelectionLifecycle: runComboSelectionLifecycle,
        applyFilteredTreeToViewModel: applyFilteredTreeToViewModel
    };
});
