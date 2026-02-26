sap.ui.define([], function () {
    "use strict";

    function runConvergenceLifecycle(mArgs) {
        var oDataModel = mArgs && mArgs.dataModel;
        if (!oDataModel || typeof oDataModel.getProperty !== "function") {
            return { ok: false, reason: "missing_data_model" };
        }

        var fnApplySummary = mArgs && mArgs.applySummary;
        if (typeof fnApplySummary !== "function") {
            return { ok: false, reason: "missing_presenter" };
        }

        var aRows = oDataModel.getProperty("/visibleCheckLists") || [];
        var iTotal = (oDataModel.getProperty("/checkLists") || []).length;
        if (!Array.isArray(aRows)) {
            return { ok: false, reason: "malformed_rows" };
        }

        fnApplySummary(aRows, iTotal);

        var fnApplyEmptyState = mArgs && mArgs.applyEmptyState;
        if (typeof fnApplyEmptyState === "function") {
            fnApplyEmptyState();
        }

        return { ok: true, reason: "convergence_applied", visible: aRows.length, total: iTotal };
    }

    return {
        runConvergenceLifecycle: runConvergenceLifecycle
    };
});
