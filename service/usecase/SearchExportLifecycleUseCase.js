sap.ui.define([], function () {
    "use strict";

    function runExportExecutionLifecycle(mArgs) {
        var fnRunLifecycle = mArgs && mArgs.runLifecycle;
        if (typeof fnRunLifecycle !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_export_adapter" });
        }

        return Promise.resolve().then(fnRunLifecycle).then(function (oResult) {
            return { ok: true, reason: "export_executed", result: oResult || null };
        }).catch(function (oError) {
            return {
                ok: false,
                reason: "export_failed",
                error: (oError && oError.message) || "export_failed"
            };
        });
    }

    function runIntentPresentationLifecycle(mArgs) {
        var oResult = mArgs && mArgs.result;
        var fnPresent = mArgs && mArgs.present;
        if (typeof fnPresent !== "function") {
            return { ok: false, reason: "missing_presenter", result: oResult || null };
        }

        var oPresentation = fnPresent(oResult);
        var fnOnUnexpected = mArgs && mArgs.onUnexpected;
        var bUnexpected = !!(oPresentation && (
            oPresentation.reason === "unknown_result_reason"
            || oPresentation.reason === "presentation_failed_disabled"
            || oPresentation.reason === "presentation_failed_error"
        ));

        if (bUnexpected && typeof fnOnUnexpected === "function") {
            fnOnUnexpected(oPresentation);
        }

        return {
            ok: true,
            reason: bUnexpected ? "presented_with_fallback" : "presented",
            presentation: oPresentation || null,
            result: oResult || null
        };
    }

    return {
        runExportExecutionLifecycle: runExportExecutionLifecycle,
        runIntentPresentationLifecycle: runIntentPresentationLifecycle
    };
});
