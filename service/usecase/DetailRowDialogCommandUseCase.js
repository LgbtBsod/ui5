sap.ui.define([], function () {
    "use strict";

    function resolveRowPath(sType) {
        return sType === "checks" ? "/checks" : "/barriers";
    }

    function resolveExpandedDialogId(sType) {
        return sType === "checks" ? "checksExpandedDialog" : "barriersExpandedDialog";
    }

    function shouldProcessRowDeleteResult(oResult) {
        return !!(oResult && oResult.deleted);
    }

    return {
        resolveRowPath: resolveRowPath,
        resolveExpandedDialogId: resolveExpandedDialogId,
        shouldProcessRowDeleteResult: shouldProcessRowDeleteResult
    };
});
