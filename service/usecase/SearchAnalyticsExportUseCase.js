sap.ui.define([], function () {
    "use strict";

    function openDialog(oDialog, fnAfterOpen) {
        if (!oDialog) {
            return;
        }
        oDialog.open();
        if (typeof fnAfterOpen === "function") {
            fnAfterOpen();
        }
    }

    function closeDialog(oDialog) {
        if (!oDialog) {
            return;
        }
        oDialog.close();
    }

    function buildExportPromise(sEntity, fnScreenExport, fnRemoteExport) {
        if (sEntity === "screen") {
            return Promise.resolve({ rows: fnScreenExport() });
        }
        return fnRemoteExport();
    }

    return {
        openDialog: openDialog,
        closeDialog: closeDialog,
        buildExportPromise: buildExportPromise
    };
});
