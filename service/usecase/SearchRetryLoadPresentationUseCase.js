sap.ui.define([], function () {
    "use strict";

    function normalizeRows(vRows) {
        return Array.isArray(vRows) ? vRows : [];
    }

    function resetLoadErrorState(oStateModel) {
        if (!oStateModel || typeof oStateModel.setProperty !== "function") {
            return false;
        }
        oStateModel.setProperty("/loadError", false);
        oStateModel.setProperty("/loadErrorMessage", "");
        return true;
    }

    function applyLoadSuccess(mArgs) {
        var oDataModel = mArgs && mArgs.dataModel;
        if (!oDataModel || typeof oDataModel.setProperty !== "function") {
            return false;
        }

        var aRows = normalizeRows(mArgs && mArgs.rows);
        oDataModel.setProperty("/checkLists", aRows);
        oDataModel.setProperty("/visibleCheckLists", aRows);

        if (typeof (mArgs && mArgs.onAfterApply) === "function") {
            mArgs.onAfterApply(aRows);
        }
        return true;
    }

    function applyLoadError(mArgs) {
        var oStateModel = mArgs && mArgs.stateModel;
        if (!oStateModel || typeof oStateModel.setProperty !== "function") {
            return false;
        }

        var sMessage = (mArgs && mArgs.error && mArgs.error.message)
            || (mArgs && mArgs.fallbackMessage)
            || "";
        oStateModel.setProperty("/loadError", true);
        oStateModel.setProperty("/loadErrorMessage", sMessage);
        return true;
    }

    function runRetryFlow(mArgs) {
        var fnLoader = mArgs && mArgs.getCheckLists;
        var iMaxAttempts = Math.max(1, Number(mArgs && mArgs.maxAttempts) || 1);
        var iAttempt = 0;

        resetLoadErrorState(mArgs && mArgs.stateModel);

        if (typeof fnLoader !== "function") {
            applyLoadError({
                stateModel: mArgs && mArgs.stateModel,
                error: new Error("retry loader is not available")
            });
            return Promise.resolve({ ok: false, reason: "missing_loader", attempts: 0 });
        }

        var fnRunWithLoading = typeof (mArgs && mArgs.runWithLoading) === "function"
            ? mArgs.runWithLoading
            : function (fnTask) { return fnTask(); };

        function attempt() {
            iAttempt += 1;
            return Promise.resolve(fnRunWithLoading(fnLoader)).then(function (aRowsRaw) {
                var aRows = normalizeRows(aRowsRaw);
                if (mArgs && mArgs.treatEmptyAsError && aRows.length === 0) {
                    applyLoadError({
                        stateModel: mArgs.stateModel,
                        error: new Error((mArgs && mArgs.emptyRowsMessage) || "No rows loaded")
                    });
                    return { ok: false, reason: "empty_rows", attempts: iAttempt };
                }

                applyLoadSuccess({
                    dataModel: mArgs && mArgs.dataModel,
                    rows: aRows,
                    onAfterApply: mArgs && mArgs.onAfterApply
                });
                return { ok: true, rows: aRows, attempts: iAttempt };
            }).catch(function (oError) {
                if (iAttempt < iMaxAttempts) {
                    return attempt();
                }
                applyLoadError({
                    stateModel: mArgs && mArgs.stateModel,
                    error: oError
                });
                return { ok: false, reason: "error", attempts: iAttempt, error: oError };
            });
        }

        return attempt();
    }

    return {
        normalizeRows: normalizeRows,
        resetLoadErrorState: resetLoadErrorState,
        applyLoadSuccess: applyLoadSuccess,
        applyLoadError: applyLoadError,
        runRetryFlow: runRetryFlow
    };
});
