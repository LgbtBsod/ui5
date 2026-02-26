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


    function runExportIntentOrchestration(mArgs) {
        return mArgs.runExportIntentLifecycle({
            runIntent: function () {
                return mArgs.runExportIntent({
                    event: mArgs.event,
                    source: mArgs.source,
                    defaultEntity: "screen",
                    allowedEntities: ["screen", "barrier", "check"],
                    resolveEntityFromMenuEvent: mArgs.resolveEntityFromMenuEvent,
                    isEnabled: mArgs.isEnabled,
                    runExport: mArgs.runExport
                });
            },
            presentIntentResult: mArgs.presentIntentResult
        }).then(function (oLifecycleResult) {
            return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
        });
    }


    function runExportExecutionOrchestration(mArgs) {
        return runExportExecutionLifecycle({
            runLifecycle: function () {
                return mArgs.runExportLifecycle({
                    runExportFlow: mArgs.runExportFlow,
                    runWithLoading: mArgs.runWithLoading,
                    buildExportPromise: mArgs.buildExportPromise,
                    onEmpty: mArgs.onEmpty,
                    onSuccess: mArgs.onSuccess,
                    onError: mArgs.onError
                });
            }
        }).then(function (oLifecycleResult) {
            return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
        });
    }

    function runExportExecutionPresentationOrchestration(mArgs) {
        return runExportExecutionOrchestration({
            runExportLifecycle: mArgs && mArgs.runExportLifecycle,
            runExportFlow: mArgs && mArgs.runExportFlow,
            runWithLoading: mArgs && mArgs.runWithLoading,
            buildExportPromise: mArgs && mArgs.buildExportPromise,
            onEmpty: function () {
                if (typeof (mArgs && mArgs.showToast) === "function") {
                    mArgs.showToast(mArgs.getText("exportEmpty"));
                }
            },
            onSuccess: function (aRows) {
                if (typeof (mArgs && mArgs.download) === "function") {
                    mArgs.download(mArgs.buildFilename(mArgs && mArgs.entity), aRows || []);
                }
                if (typeof (mArgs && mArgs.showToast) === "function") {
                    mArgs.showToast(mArgs.getText("exportDone", [(aRows || []).length]));
                }
            },
            onError: function (oError) {
                if (typeof (mArgs && mArgs.showToast) === "function") {
                    mArgs.showToast(mArgs.getText("exportFailed", [((oError && oError.message) || "Unknown error")]));
                }
            }
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


    function runExportIntentPresentationOrchestration(mArgs) {
        runIntentPresentationLifecycle({
            result: mArgs && mArgs.result,
            present: mArgs && mArgs.present,
            onUnexpected: mArgs && mArgs.onUnexpected
        });
        return (mArgs && mArgs.result) || null;
    }


    return {
        runExportExecutionLifecycle: runExportExecutionLifecycle,
        runExportIntentOrchestration: runExportIntentOrchestration,
        runExportExecutionOrchestration: runExportExecutionOrchestration,
        runExportExecutionPresentationOrchestration: runExportExecutionPresentationOrchestration,
        runIntentPresentationLifecycle: runIntentPresentationLifecycle,
        runExportIntentPresentationOrchestration: runExportIntentPresentationOrchestration
    };
});
