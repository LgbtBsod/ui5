sap.ui.define([
    "sap_ui5/service/usecase/DetailStatusRowUseCase",
    "sap_ui5/service/usecase/DetailDialogLifecycleUseCase",
    "sap_ui5/service/usecase/DetailRowDialogCommandUseCase",
    "sap_ui5/service/usecase/DetailStatusCommandUseCase",
    "sap_ui5/util/RowListHelper"
], function (DetailStatusRowUseCase, DetailDialogLifecycleUseCase, DetailRowDialogCommandUseCase, DetailStatusCommandUseCase, RowListHelper) {
    "use strict";

    function openExpandedDialogByType(mArgs) {
        var sType = mArgs && mArgs.type;
        var oMeta = DetailStatusRowUseCase.resolveExpandedDialogMeta(sType);
        return DetailDialogLifecycleUseCase.openDialog({
            host: mArgs.host,
            prop: oMeta.prop,
            fragment: oMeta.fragment,
            view: mArgs.view,
            controller: mArgs.controller
        });
    }

    function closeExpandedDialogByType(mArgs) {
        var sDialogId = DetailRowDialogCommandUseCase.resolveExpandedDialogId(mArgs && mArgs.type);
        var oDialog = mArgs && typeof mArgs.byId === "function" ? mArgs.byId(sDialogId) : null;
        if (oDialog && typeof oDialog.close === "function") {
            oDialog.close();
        }
    }

    function addRowByType(mArgs) {
        var sPath = DetailRowDialogCommandUseCase.resolveRowPath(mArgs && mArgs.type);
        return mutateRows({
            selectedModel: mArgs.selectedModel,
            path: sPath,
            mutator: RowListHelper.addRow,
            onMutated: mArgs.onMutated
        });
    }

    function deleteRowByType(mArgs) {
        var sPath = DetailRowDialogCommandUseCase.resolveRowPath(mArgs && mArgs.type);
        var oResult = mArgs.deleteRowFromEvent(mArgs.event, "selected", sPath);
        if (!DetailRowDialogCommandUseCase.shouldProcessRowDeleteResult(oResult)) {
            return oResult;
        }
        DetailStatusCommandUseCase.handleDeleteRowResult(
            oResult,
            DetailStatusRowUseCase.shouldSyncAfterDeleteResult,
            mArgs.onSyncSelectionMeta
        );
        return oResult;
    }

    function mutateRows(mArgs) {
        var oSelectedModel = mArgs && mArgs.selectedModel;
        if (!oSelectedModel || typeof oSelectedModel.getProperty !== "function" || typeof oSelectedModel.setProperty !== "function") {
            return [];
        }
        var sPath = mArgs.path;
        var fnMutator = mArgs.mutator;
        var aRows = oSelectedModel.getProperty(sPath) || [];
        var aMutated = typeof fnMutator === "function" ? fnMutator(aRows) : aRows;
        oSelectedModel.setProperty(sPath, aMutated);
        if (typeof mArgs.onMutated === "function") {
            mArgs.onMutated();
        }
        return aMutated;
    }

    return {
        openExpandedDialogByType: openExpandedDialogByType,
        closeExpandedDialogByType: closeExpandedDialogByType,
        addRowByType: addRowByType,
        deleteRowByType: deleteRowByType,
        mutateRows: mutateRows
    };
});
