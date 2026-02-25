sap.ui.define([
    "sap_ui5/service/usecase/SearchActionUseCase"
], function (SearchActionUseCase) {
    "use strict";

    function getSelectedId(mArgs) {
        var oSelectedModel = mArgs && mArgs.selectedModel;
        var oSelected = oSelectedModel && typeof oSelectedModel.getData === "function"
            ? (oSelectedModel.getData() || {})
            : {};
        return SearchActionUseCase.extractSelectedChecklistId(oSelected);
    }

    function getRows(mArgs) {
        var oDataModel = mArgs && mArgs.dataModel;
        var aRows = oDataModel && typeof oDataModel.getProperty === "function"
            ? (oDataModel.getProperty("/visibleCheckLists") || oDataModel.getProperty("/checkLists") || [])
            : [];
        return Array.isArray(aRows) ? aRows : [];
    }

    function hasRowWithId(aRows, sId) {
        if (!sId) {
            return false;
        }
        return (aRows || []).some(function (oRow) {
            var oRoot = oRow && oRow.root;
            return !!(oRoot && oRoot.id === sId);
        });
    }

    function resolveActionState(mArgs) {
        var sSelectedId = getSelectedId(mArgs);
        var aRows = getRows(mArgs);
        var bLoading = !!(mArgs && mArgs.isLoading);
        var bUseSmartControls = mArgs && mArgs.useSmartControls !== false;

        var bSelectionInRows = hasRowWithId(aRows, sSelectedId);
        var bHasSelection = !!sSelectedId && (bUseSmartControls || bSelectionInRows);
        var bHasRows = aRows.length > 0;

        return {
            hasSelection: bHasSelection,
            canCopy: bHasSelection && !bLoading,
            canDelete: bHasSelection && !bLoading,
            canExport: bHasRows && !bLoading,
            canRetryLoad: !bLoading
        };
    }

    function applyActionStateToViewModel(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        if (!oViewModel || typeof oViewModel.setProperty !== "function") {
            return false;
        }

        var mState = resolveActionState(mArgs);
        oViewModel.setProperty("/hasSelection", mState.hasSelection);
        oViewModel.setProperty("/canCopy", mState.canCopy);
        oViewModel.setProperty("/canDelete", mState.canDelete);
        oViewModel.setProperty("/canExport", mState.canExport);
        oViewModel.setProperty("/canRetryLoad", mState.canRetryLoad);
        return true;
    }

    return {
        getSelectedId: getSelectedId,
        getRows: getRows,
        hasRowWithId: hasRowWithId,
        resolveActionState: resolveActionState,
        applyActionStateToViewModel: applyActionStateToViewModel
    };
});
