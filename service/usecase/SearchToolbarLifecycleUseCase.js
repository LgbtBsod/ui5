sap.ui.define([], function () {
    "use strict";

    function runCreateLifecycle(mArgs) {
        var fnRunCreateFlow = mArgs && mArgs.runCreateFlow;
        if (typeof fnRunCreateFlow !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_create_flow" });
        }

        return Promise.resolve().then(fnRunCreateFlow).then(function (oResult) {
            return { ok: true, reason: "create_flow_completed", result: oResult || null };
        });
    }

    function runCopyLifecycle(mArgs) {
        var fnRunCopyFlow = mArgs && mArgs.runCopyFlow;
        if (typeof fnRunCopyFlow !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_copy_flow" });
        }

        return Promise.resolve().then(fnRunCopyFlow).then(function (oResult) {
            return { ok: true, reason: "copy_flow_completed", result: oResult || null };
        });
    }

    function runDeleteLifecycle(mArgs) {
        var fnRunDeleteFlow = mArgs && mArgs.runDeleteFlow;
        if (typeof fnRunDeleteFlow !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_delete_flow" });
        }

        return Promise.resolve().then(fnRunDeleteFlow).then(function (oResult) {
            var fnPresentDeleteOutcome = mArgs && mArgs.presentDeleteOutcome;
            if (typeof fnPresentDeleteOutcome === "function") {
                fnPresentDeleteOutcome(oResult);
            }

            return { ok: true, reason: "delete_flow_completed", result: oResult || null };
        });
    }

    function runExportIntentLifecycle(mArgs) {
        var fnRunIntent = mArgs && mArgs.runIntent;
        if (typeof fnRunIntent !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_export_intent" });
        }

        return Promise.resolve().then(fnRunIntent).then(function (oResult) {
            var fnPresentIntentResult = mArgs && mArgs.presentIntentResult;
            if (typeof fnPresentIntentResult === "function") {
                fnPresentIntentResult(oResult);
            }

            return { ok: true, reason: "export_intent_completed", result: oResult || null };
        });
    }

    return {
        runCreateLifecycle: runCreateLifecycle,
        runCopyLifecycle: runCopyLifecycle,
        runDeleteLifecycle: runDeleteLifecycle,
        runExportIntentLifecycle: runExportIntentLifecycle
    };
});
