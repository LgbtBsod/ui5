sap.ui.define([], function () {
    "use strict";

    function runOpenLifecycle(mArgs) {
        var fnOpenDialog = mArgs && mArgs.openDialog;
        if (typeof fnOpenDialog !== "function") {
            return { ok: false, reason: "missing_open_adapter" };
        }

        var fnApplyDegradedState = mArgs && mArgs.applyDegradedState;
        if (typeof fnApplyDegradedState === "function") {
            fnApplyDegradedState();
        }

        fnOpenDialog();
        return { ok: true, reason: "dialog_opened" };
    }

    function runLoadLifecycle(mArgs) {
        var fnRunLoadFlow = mArgs && mArgs.runLoadFlow;
        if (typeof fnRunLoadFlow !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_load_flow" });
        }

        return Promise.resolve().then(fnRunLoadFlow).then(function (oResult) {
            return { ok: true, reason: "dialog_load_completed", result: oResult || null };
        }).catch(function (oError) {
            var fnApplyLoadError = mArgs && mArgs.applyLoadError;
            if (typeof fnApplyLoadError === "function") {
                fnApplyLoadError((oError && oError.message) || "analytics_load_failed");
            }
            return { ok: false, reason: "dialog_load_failed", error: (oError && oError.message) || "analytics_load_failed" };
        });
    }

    function runCloseLifecycle(mArgs) {
        var fnCloseDialog = mArgs && mArgs.closeDialog;
        if (typeof fnCloseDialog !== "function") {
            return { ok: false, reason: "missing_close_adapter" };
        }

        fnCloseDialog();
        return { ok: true, reason: "dialog_closed" };
    }

    return {
        runOpenLifecycle: runOpenLifecycle,
        runLoadLifecycle: runLoadLifecycle,
        runCloseLifecycle: runCloseLifecycle
    };
});
